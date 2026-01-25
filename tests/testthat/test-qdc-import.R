valid_xml <- '<?xml version="1.0" encoding="utf-8"?>
<CodeBook xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="urn:QDA-XML:codebook:0:4 Codebook.xsd" origin="ATLAS.ti Win 23.4.0" xmlns="urn:QDA-XML:codebook:1.0">
  <Codes>
    <Code guid="1FF58B12-6F6F-4B0C-8BE5-BBDC91F55294" name="code1" color="#46AA00" isCodable="true">
      <Description>description</Description>
    </Code>
  </Codes>
  <Sets>
    <Set name="code group 1" guid="ECA18D76-2B8B-4F26-BC1D-19ECA19FB3F0">
      <Description>kategorie 1</Description>
      <MemberCode guid="1FF58B12-6F6F-4B0C-8BE5-BBDC91F55294" />
    </Set>
  </Sets>
</CodeBook>'

valid_path <- tempfile("valid.qdc")
writeLines(valid_xml, con = valid_path)

expected_import <- list(
  codebook = tibble::tibble(
    code_name = "code1",
    code_description = "description",
    code_color = "rgb(70,170,0)",
    code_guid = "1FF58B12-6F6F-4B0C-8BE5-BBDC91F55294",
    parent_guid = NA_character_,
    category_guids = list("ECA18D76-2B8B-4F26-BC1D-19ECA19FB3F0")
  ),
  categories = tibble::tibble(
    category_name = "code group 1",
    category_description = "kategorie 1",
    category_guid = "ECA18D76-2B8B-4F26-BC1D-19ECA19FB3F0"
  ),
  message = NULL
)

test_that(
  "importing QDC returns expected list of data.frames",
  expect_equal(
    parse_qdc(valid_path),
    expected_import
  )
)

# unlink(system.file("valid.qdc", package = "requal"))

corrupted_xml <- '<?xml version="1.0" encoding="utf-8"?>
<CodeBook xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="urn:QDA-XML:codebook:0:4 Codebook.xsd" origin="ATLAS.ti Win 23.4.0" xmlns="urn:QDA-XML:codebook:0:4">
  <Codes>
    <Code guid="1FF58B12-6F6F-4B0C-8BE5-BBDC91F55294" name="code1" color="#46AA00" isCodable="true">
      <Description>popisek - nebo přesněji Comment - kódu 1</Description>
    </Code>
    <Code guid="B52FB733-B6AF-4B31-BB78-3E733135171A" name="code2" color="#FF7800" isCodable="true">
      <Description>popisek kódu 2</Description>
    </Code>
    <Code guid="51191CEE-BF78-4E92-9DF8-3AE66721E038" name="code3" color="#006EFF" isCodable="true">
      <Description>popisek kódu 3</Description>
      <Code guid="14C4E248-F726-42CB-B1CD-A65D6A86F296" name="subcode 3.1" isCodable="true" />
    </Code>
    <Code guid="9FCD0F84-AA08-4765-B1B1-57A39966042A" name="subcode 2.1" isCodable="true">
      <Description>chtěl jsem vytvořit subkód code2, ale nějak mi to nešlo, nabízelo mi to jen merge nebo link</Description>
    </Code>
    <Code 
'

corrupted_path <- tempfile("corrupted.qdc")
writeLines(
  corrupted_xml,
  con = corrupted_path
)

corrupted_import <- parse_qdc(corrupted_path)

test_that("importing QDC returns empty data.frame when qdc export is corrupted", {
  expect_equal(
    corrupted_import$codebook,
    data.frame()
  )
  expect_equal(
    corrupted_import$categories,
    data.frame()
  )
  expect_true(
    !is.null(corrupted_import$message)
  )
})

unlink(valid_path)
unlink(corrupted_path)
