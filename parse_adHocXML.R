library(xml2)

extract_dateAndISIN = function (message) {
  fields = xml_find_all(message, "field")
  for (f in fields) {
    if (xml_attrs(f) == "dtcreated") {
      date = xml_text((f))
    } else if (xml_attrs(f) == "isin") {
      isin = xml_text((f))
    }
  }
  
  if (!exists("date")) {
    # Ignore this message if date extraction failed.
    warning("Could not extract a date")
    print("Could not extract a date")
    return(NULL)
  }
  
  if (exists("isin")) {
    # Successfully extracted date and isin.
    return(cbind(date, isin))
  }
  
  # Not all AdHoc messages contain a node "field name='isin'"
  # If no such node is present we must extract it from 'headline' or 'body'.
  for (f in fields) {
    if (xml_attrs(f) == "headline") {
      headline = xml_text((f))
    } else if (xml_attrs(f) == "body") {
      body = xml_text((f))
    }
  }
  
  # regular expression of ISIN:
  # expr = "[a-zA-Z]{2}[ ]?[a-zA-Z0-9]{9}[0-9]{1}"
  expr = paste("(XS|AD|AE|AF|AG|AI|AL|AM|AO|AQ|AR|AS|AT|AU|AW|AX|AZ|BA|BB|BD|",
               "BE|BF|BG|BH|BI|BJ|BL|BM|BN|BO|BQ|BR|BS|BT|BV|BW|BY|BZ|CA|CC|",
               "CD|CF|CG|CH|CI|CK|CL|CM|CN|CO|CR|CU|CV|CW|CX|CY|CZ|DE|DJ|DK|",
               "DM|DO|DZ|EC|EE|EG|EH|ER|ES|ET|FI|FJ|FK|FM|FO|FR|GA|GB|GD|GE|",
               "GF|GG|GH|GI|GL|GM|GN|GP|GQ|GR|GS|GT|GU|GW|GY|HK|HM|HN|HR|HT|",
               "HU|ID|IE|IL|IM|IN|IO|IQ|IR|IS|IT|JE|JM|JO|JP|KE|KG|KH|KI|KM|",
               "KN|KP|KR|KW|KY|KZ|LA|LB|LC|LI|LK|LR|LS|LT|LU|LV|LY|MA|MC|MD|",
               "ME|MF|MG|MH|MK|ML|MM|MN|MO|MP|MQ|MR|MS|MT|MU|MV|MW|MX|MY|MZ|",
               "NA|NC|NE|NF|NG|NI|NL|NO|NP|NR|NU|NZ|OM|PA|PE|PF|PG|PH|PK|PL|",
               "PM|PN|PR|PS|PT|PW|PY|QA|RE|RO|RS|RU|RW|SA|SB|SC|SD|SE|SG|SH|",
               "SI|SJ|SK|SL|SM|SN|SO|SR|SS|ST|SV|SX|SY|SZ|TC|TD|TF|TG|TH|TJ|",
               "TK|TL|TM|TN|TO|TR|TT|TV|TW|TZ|UA|UG|UM|US|UY|UZ|VA|VC|VE|VG|",
               "VI|VN|VU|WF|WS|YE|YT|ZA|ZM|ZW)",
               "([0-9A-Z]{9})",
               "([0-9]{1})")
  # 2 characters:   country
  # 9 alphanumeric: NSIN
  # 1 number:       checksum
  
  
  # extrakt ISIN
  # check for ISIN in field 'body'
  isin_idx = regexpr(expr, body)
  if (isin_idx > 0)  {
    isin = regmatches(body, isin_idx)
  }
  else {
    # check for ISIN in field 'headline'
    isin_idx = regexpr(expr, headline)
    if (isin_idx > 0) {
      isin = regmatches(headline, isin_idx)
    }
    else {
      # either there was absolutely no ISIN in the specified nodes, 
      # or the ISIN had a length of only 11 characters (missing checksum)
      warning("Could not extract ISIN")
      print("Could not extract ISIN:")
      print(date)
      print(headline)


      return(NULL)
      
      # Examples where this happens (file: DGAP_Ad-hoc_04_05.xml):
      # 2004-01-22 16:07:08"
      # [1] "Die ist ein Testmeldung. Bitte ignorieren! "
      # 
      # 2005-09-30 17:53:22"
      # [1] "VKW-Halbjahresergebnis 2005; VKW erwirbt Mehrheit an der 'VEG' "
      #
      # US456661060 (missing checksum/only 11 characters)
      
    }
  }
  
  return(cbind(date, isin))
}

# read xml file
files = c("data/DGAP_Ad-hoc/DGAP_Ad-hoc_02-04.xml",
          "data/DGAP_Ad-hoc/DGAP_Ad-hoc_04_05.xml",
          "data/DGAP_Ad-hoc/DGAP_Ad-hoc_05-06.xml",
          "data/DGAP_Ad-hoc/DGAP_Ad-hoc_06-07.xml",
          "data/DGAP_Ad-hoc/DGAP_Ad-hoc_07_Aug10.xml",
          "data/DGAP_Ad-hoc/DGAP_Ad-hoc_Apr11_Jun11.xml",
          "data/DGAP_Ad-hoc/DGAP_Ad-hoc_Sep10_Apr11.xml")

rows = xml_children(read_xml(files[6]))

events = c()
for (x in rows) {
  e = extract_dateAndISIN(x)
  events = rbind(events, e)
  remove(e, x)
}

ev = data.frame(events)

# sort stuff
library(dplyr)
ev = arrange(ev, isin, date)
print(ev)
