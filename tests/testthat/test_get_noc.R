# Tests for get_noc.R.
#
# get_noc() downloads the National Operator Codes register. Rather than skip
# it as a network test, these tests point it at a cut-down register written to
# a temporary file: download.file() takes a file:// URL, so everything after
# the download is exercised for real.

context("The National Operator Codes register")

file_url <- function(path) {
  p <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (substr(p, 1, 1) == "/") paste0("file://", p) else paste0("file:///", p)
}

# Five codes under three operators, two management divisions and one group.
# OLDC has ceased, one record has no code at all, and ORPH's operator is
# missing from the Operators section.
noc_xml <- function(include_operators = TRUE) {
  paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n<travelinedata>\n<NOCTable>\n',
    '<NOCTableRecord><NOCCODE>SCYO</NOCCODE>',
    '<OperatorPublicName>Stagecoach Yorkshire</OperatorPublicName>',
    '<VOSA_PSVLicenseName>YORKSHIRE TRACTION COMPANY LIMITED</VOSA_PSVLicenseName>',
    '<OpId>200</OpId><DateCeased/></NOCTableRecord>\n',
    '<NOCTableRecord><NOCCODE>ELBG</NOCCODE>',
    '<OperatorPublicName>Stagecoach London</OperatorPublicName>',
    '<VOSA_PSVLicenseName>EAST LONDON BUS &amp; COACH COMPANY LIMITED</VOSA_PSVLicenseName>',
    '<OpId>100</OpId><DateCeased/></NOCTableRecord>\n',
    '<NOCTableRecord><NOCCODE>OLDC</NOCCODE>',
    '<OperatorPublicName>Old Coaches</OperatorPublicName>',
    '<VOSA_PSVLicenseName>OLD COACHES LTD</VOSA_PSVLicenseName>',
    '<OpId>300</OpId><DateCeased>2019-01-01</DateCeased></NOCTableRecord>\n',
    '<NOCTableRecord><NOCCODE></NOCCODE>',
    '<OperatorPublicName>No Code At All</OperatorPublicName>',
    '<VOSA_PSVLicenseName>NO CODE LTD</VOSA_PSVLicenseName>',
    '<OpId>100</OpId><DateCeased/></NOCTableRecord>\n',
    '<NOCTableRecord><NOCCODE>ORPH</NOCCODE>',
    '<OperatorPublicName>Orphan Buses</OperatorPublicName>',
    '<VOSA_PSVLicenseName>ORPHAN BUSES LTD</VOSA_PSVLicenseName>',
    '<OpId>999</OpId></NOCTableRecord>\n',
    '</NOCTable>\n',
    if (include_operators) paste0(
      '<Operators>\n',
      '<OperatorsRecord><OpId>100</OpId>',
      '<OpNm>East London Bus &amp; Coach Co Ltd</OpNm>',
      '<ManDivId>10</ManDivId></OperatorsRecord>\n',
      '<OperatorsRecord><OpId>200</OpId>',
      '<OpNm>Yorkshire Traction Co Ltd</OpNm>',
      '<ManDivId>20</ManDivId></OperatorsRecord>\n',
      '<OperatorsRecord><OpId>300</OpId><OpNm>Old Coaches Ltd</OpNm>',
      '<ManDivId>10</ManDivId></OperatorsRecord>\n',
      '</Operators>\n') else "",
    '<ManagementDivisions>\n',
    '<ManagementDivisionsRecord><ManDivId>10</ManDivId>',
    '<ManDivNm>London</ManDivNm><GpId>1</GpId></ManagementDivisionsRecord>\n',
    '<ManagementDivisionsRecord><ManDivId>20</ManDivId>',
    '<ManDivNm>Yorkshire</ManDivNm><GpId>1</GpId></ManagementDivisionsRecord>\n',
    '</ManagementDivisions>\n',
    '<Groups>\n',
    '<GroupsRecord><GpId>1</GpId><GpNme>Stagecoach Group</GpNme></GroupsRecord>\n',
    '</Groups>\n</travelinedata>\n')
}

write_noc <- function(xml, name = "nocrecords.xml") {
  path <- file.path(tempdir(), name)
  writeLines(xml, path, useBytes = TRUE)
  path
}


test_that("get_noc returns one row per code with its operator hierarchy", {
  noc <- get_noc(url = file_url(write_noc(noc_xml())))

  expect_s3_class(noc, "data.frame")
  expect_named(noc, c("noc", "public_name", "licence_name", "operator_id",
                      "operator_name", "division_id", "division", "group_id",
                      "group"))
  # ceased and code-less records are dropped, and the result is sorted by code
  expect_equal(noc$noc, c("ELBG", "ORPH", "SCYO"))

  elbg <- noc[noc$noc == "ELBG", ]
  expect_equal(elbg$public_name, "Stagecoach London")
  expect_equal(elbg$licence_name, "EAST LONDON BUS & COACH COMPANY LIMITED")
  expect_equal(elbg$operator_id, "100")
  expect_equal(elbg$operator_name, "East London Bus & Coach Co Ltd")
  expect_equal(elbg$division, "London")
  expect_equal(elbg$group, "Stagecoach Group")

  # two codes in the same group but different divisions are different operators
  scyo <- noc[noc$noc == "SCYO", ]
  expect_equal(scyo$group, "Stagecoach Group")
  expect_equal(scyo$division, "Yorkshire")
  expect_false(scyo$operator_id == elbg$operator_id)
})


test_that("get_noc keeps a code whose operator is not in the register", {
  noc <- get_noc(url = file_url(write_noc(noc_xml())))
  orph <- noc[noc$noc == "ORPH", ]

  expect_equal(orph$public_name, "Orphan Buses")
  expect_true(is.na(orph$operator_name))
  expect_true(is.na(orph$division))
  expect_true(is.na(orph$group))
})


test_that("get_noc can include ceased codes", {
  noc <- get_noc(url = file_url(write_noc(noc_xml())), include_ceased = TRUE)

  expect_equal(noc$noc, c("ELBG", "OLDC", "ORPH", "SCYO"))
  expect_equal(noc$operator_name[noc$noc == "OLDC"], "Old Coaches Ltd")
})


test_that("get_noc rejects a register with a section missing", {
  path <- write_noc(noc_xml(include_operators = FALSE), "noc_no_ops.xml")
  expect_error(get_noc(url = file_url(path)),
               "the NOC database has no Operators section")
})


test_that("get_noc reads the Windows-1252 bytes the feed really contains", {
  # The register declares UTF-8 and is not: operator names carry Windows-1252
  # smart quotes. Parsing it as UTF-8 stops dead, so get_noc overrides the
  # declared encoding.
  xml <- noc_xml()
  xml <- sub("Orphan Buses", "Orphan«QUOTE»s Buses", xml)
  raw_xml <- charToRaw(enc2utf8(xml))
  # replace the placeholder with a bare 0x92 byte, which is a right single
  # quotation mark in Windows-1252 and invalid UTF-8
  marker <- charToRaw("«QUOTE»")
  start <- which(sapply(seq_len(length(raw_xml) - length(marker) + 1),
                        function(i) {
                          all(raw_xml[i:(i + length(marker) - 1)] == marker)
                        }))[1]
  raw_xml <- c(raw_xml[seq_len(start - 1)], as.raw(0x92),
               raw_xml[(start + length(marker)):length(raw_xml)])

  path <- file.path(tempdir(), "noc_1252.xml")
  con <- file(path, "wb")
  writeBin(raw_xml, con)
  close(con)

  noc <- get_noc(url = file_url(path))
  expect_equal(noc$public_name[noc$noc == "ORPH"], "Orphan’s Buses")
})


test_that("get_noc leaves no temporary files behind", {
  before <- list.files(tempdir())
  get_noc(url = file_url(write_noc(noc_xml())))
  expect_false("temp_noc" %in% setdiff(list.files(tempdir()), before))
})


test_that("noc_normalise_name ignores everything that is not the name", {
  expect_equal(noc_normalise_name("EAST LONDON BUS & COACH COMPANY LIMITED"),
               noc_normalise_name("East London Bus & Coach Co Ltd"))
  expect_equal(noc_normalise_name("Arriva Yorkshire Ltd."),
               noc_normalise_name("ARRIVA  YORKSHIRE  PLC"))
  expect_equal(noc_normalise_name(c(NA, "")), c("", ""))
  expect_equal(noc_normalise_name(factor("First Leeds")), "first leeds")
})


test_that("noc_operator_key resolves codes through a real register", {
  noc <- get_noc(url = file_url(write_noc(noc_xml())))

  # by code, and by any of the three names the register knows
  k <- noc_operator_key(c("ELBG", "unknown-id", "unknown-id"),
                        c("", "Stagecoach London",
                          "EAST LONDON BUS & COACH COMPANY LIMITED"), noc)
  expect_equal(length(unique(k)), 1L)
  expect_match(k[1], "operator")

  # an id that resolves nothing keeps its own identity
  k <- noc_operator_key(c("ZZ1", "ZZ2"), c("Nobody", "Nobody"), noc)
  expect_false(k[1] == k[2])
  expect_match(k[1], "agency_id")

  # no register at all means every record stands alone
  k <- noc_operator_key(c("ELBG", "ELBG"), c("x", "y"), NULL)
  expect_equal(k[1], k[2])
  expect_match(k[1], "agency_id")
  expect_equal(noc_operator_key("ELBG", "x", noc[0, ]),
               noc_operator_key("ELBG", "x", NULL))
})
