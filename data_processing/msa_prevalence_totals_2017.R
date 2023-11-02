#library(locations)

################################################################################
          ##Create data for which counties are in each MSA##
##Used this to determine which counties comprise each MSA#
################################################################################
# msa_names = data.list.clean.prevalence[[36]]
# msa_names = msa_names[[2]]
# 
# msa_consolidated <- msa_names%>%
#   count(location)
# 
# msa_vector = msa_consolidated[['location']]
# 
# need = locations::get.contained.locations(
#   msa_vector,
#   sub.type= "county",
#   return.list = T,
#   throw.error.if.unregistered.type = T
# )

################################################################################
#Create County totals of prevalence to estimate MSAs for 2017 only#
################################################################################
data.list.msa.estprev = lapply(data.list.clean.prevalence, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  if(grepl("state", filename)) {
    data$type = "state"
  }
  if(grepl("ehe", filename)) {
    data$type = "ehe"
  }
  if(grepl("msa", filename)) {
    data$type = "msa"
  }
  if(grepl("allcounty", filename)) {
    data$type = "county"
  } 
  data <- data %>%
    filter(type == "county")%>%  #removing anything that isn't county data
    filter(year == "2017")  #remove anything that isn't 2017 data
  
  data$msa_estimate = case_when(
    data$location == '39133' | data$location == "39153" ~ "C.10420",
    data$location == '36001' | data$location == "36083"  | data$location == "36091"| data$location == "36093"| data$location == "36095" ~ "C.10580", 
    data$location == '35001'| data$location ==  '35043' |data$location == '35057'|data$location == '35061' ~"C.10740",
    data$location == '34041' | data$location =='42025'| data$location == '42077'| data$location == '42095' ~"C.10900",
    data$location =='13013'|data$location == '13015'|data$location ==  '13035'|data$location == '13045'|data$location == '13057'|data$location == '13063' |data$location =='13067'|data$location == '13077'|data$location == '13085'|data$location == '13089'|data$location == '13097'|data$location == '13113' |data$location =='13117'|data$location == '13121' |data$location =='13135'|data$location == '13143' |data$location =='13149' |data$location =='13151'|data$location == '13159'|data$location == '13171'|data$location == '13199'|data$location == '13211' |data$location =='13217'|data$location == '13223'| data$location == '13227' |data$location =='13231' | data$location =='13247'|data$location == '13255'|data$location == '13297'~"C.12060",
    data$location == '13033' | data$location == '13073'| data$location == '13181'| data$location == '13189'|data$location == '13245'|data$location == '45003' |data$location =='45037'~"C.12260",
    data$location == '48021'|data$location == '48055' |data$location == '48209'|data$location == '48453'|data$location == '48491'~"C.12420",
    data$location == '06029' ~"C.12540",
    data$location == '24003'|data$location == '24005' |data$location == '24013'|data$location == '24025'|data$location == '24027'|data$location == '24035'|data$location == '24510'~"C.12580",
    data$location == '22005' |data$location == '22007' |data$location =='22033'|data$location == '22037' |data$location =='22047'|data$location == '22063' |data$location =='22077'|data$location == '22091' |data$location =='22121'|data$location == '22125'~"C.12940",
    data$location ==  '01007'|data$location == '01009'|data$location == '01021'|data$location == '01073'|data$location == '01115'|data$location == '01117'~"C.13820",
    data$location ==  '16001'|data$location == '16015'|data$location == '16027'|data$location == '16045' |data$location =='16073' ~"C.14260",
    data$location == '25021'| data$location =='25023'|data$location == '25025'|data$location == '25009' |data$location =='25017' |data$location =='33015' |data$location =='33017'~"C.14460",
    data$location == '09001' ~"C.14860",
    data$location == '36029'|data$location =='36063' ~"C.15380",
    data$location == '12071' ~"C.15980",
    data$location == '45015'|data$location == '45019'|data$location == '45035' ~"C.16700",
    data$location ==  '37007' |data$location =='37025'|data$location =='37071'|data$location == '37097' |data$location =='37109' |data$location =='37119' |data$location =='37159'|data$location == '37179' |data$location =='45023'|data$location == '45057' |data$location =='45091'~"C.16740",
    data$location == '13047'|data$location == '13083'|data$location =='13295'|data$location == '47065'|data$location == '47115' |data$location =='47153'~"C.16860",
    data$location == '17031'|data$location == '17043' |data$location =='17063'|data$location == '17111' |data$location =='17197' |data$location =='17037' |data$location =='17089' |data$location =='17093' |data$location =='18073'|data$location == '18089'|data$location == '18111'|data$location == '18127'|data$location == '17097' |data$location =='55059'~"C.16980",
    data$location == '18029' |data$location =='18047'|data$location == '18115'|data$location == '18161' |data$location =='21015' |data$location =='21023'|data$location == '21037' |data$location =='21077' |data$location =='21081' |data$location =='21117' |data$location =='21191' |data$location =='39015'|data$location == '39017'|data$location == '39025' |data$location =='39061' |data$location =='39165'~"C.17140",
    data$location == '39035'|data$location == '39055'|data$location =='39085' |data$location =='39093'|data$location == '39103' ~"C.17460",
    data$location =='08041'|data$location == '08119'  ~"C.17820",
    data$location == '45017'|data$location =='45039' |data$location =='45055' |data$location =='45063'|data$location == '45079'|data$location == '45081'~"C.17900",
    data$location == '39041'|data$location == '39045' |data$location =='39049'|data$location =='39073'|data$location == '39089'|data$location == '39097'|data$location == '39117'|data$location == '39127' |data$location =='39129' |data$location =='39159'~"C.18140",
    data$location == '48085'|data$location == '48113' |data$location == '48121' |data$location =='48139' |data$location =='48231' |data$location =='48257' |data$location =='48397'|data$location == '48251'|data$location == '48367' |data$location =='48439'|data$location == '48497'~"C.19100",
    data$location == '39057'|data$location == '39109' |data$location =='39113'  ~"C.19430",
    data$location == '12035'|data$location == '12127' ~"C.19660",
    data$location == '08001'|data$location == '08005'|data$location ==  '08014'|data$location == '08019' |data$location =='08031'|data$location == '08035' |data$location =='08039' |data$location =='08047'|data$location == '08059' |data$location == '08093'~"C.19740",
    data$location =='19049' |data$location == '19077' |data$location =='19099' |data$location =='19121' |data$location =='19153'|data$location == '19181' ~"C.19780",
    data$location == '26163'|data$location == '26087' |data$location =='26093' |data$location =='26099' |data$location =='26125' |data$location =='26147' ~"C.19820",
    data$location == '37037' |data$location =='37063'|data$location =='37077' |data$location =='37135'|data$location == '37145'~"C.20500",
    data$location == '48141'|data$location == '48229' ~"C.21340",
    data$location == '37051'|data$location =='37085' |data$location == '37093'~"C.22180",
    data$location == '05007'|data$location == '05087'|data$location == '05143' ~"C.22220",
    data$location ==  '06019'~"C.23420",
    data$location == '26067'|data$location =='26081'|data$location == '26117' |data$location =='26139'~"C.24340",
    data$location == '37081'|data$location == '37151'|data$location =='37157' ~"C.24660",
    data$location == '45007'|data$location == '45045'|data$location =='45059' |data$location =='45077'~"C.24860",
    data$location == '42041'|data$location == '42043' |data$location =='42099' ~"C.25420",
    data$location == '09003'|data$location == '09007' |data$location =='09013'  ~"C.25540",
    data$location == '48015'|data$location =='48039'|data$location =='48071'|data$location == '48157' |data$location =='48167'|data$location == '48201' |data$location =='48291' |data$location =='48339'|data$location =='48473'~"C.26420",
    data$location ==  '01083'|data$location == '01089' ~"C.26620",
    data$location == '18011' |data$location =='18013'|data$location == '18057' |data$location == '18059'|data$location == '18063' |data$location =='18081'|data$location == '18095'|data$location == '18097'|data$location == '18109'|data$location ==  '18133' |data$location =='18145' ~"C.26900",
    data$location == '28029' |data$location =='28049'|data$location == '28051'|data$location == '28089' |data$location == '28121'|data$location == '28127'|data$location == '28163'~"C.27140",
    data$location == '12003'|data$location == '12019'|data$location == '12031' |data$location =='12089' |data$location =='12109' ~"C.27260",
    data$location == '20091'|data$location == '20103'|data$location == '20107' |data$location =='20121'|data$location == '20209' |data$location =='29013'|data$location == '29025' |data$location =='29037'|data$location == '29047'|data$location == '29049' |data$location =='29095' |data$location =='29107' |data$location =='29165' |data$location =='29177'~"C.28140",
    data$location == '47001' |data$location == '47009'|data$location == '47013' |data$location =='47093'|data$location =='47105' |data$location =='47129' |data$location =='47145' |data$location =='47173'~"C.28940",
    data$location =='12105' ~"C.29460",
    data$location ==  '42071' ~"C.29540",
    data$location == '26037'|data$location == '26045'| data$location =='26065'|data$location =='26155'~"C.29620",
    data$location == '32003' ~"C.29820",
    data$location == '21017'|data$location == '21049'|data$location == '21067'|data$location == '21113' |data$location =='21209'|data$location == '21239'  ~"C.30460",
    data$location == '05045' |data$location =='05053'|data$location =='05085' |data$location =='05105' |data$location =='05119'|data$location == '05125' ~"C.30780",
    data$location == '06059' |data$location =='06037'  ~"C.31080",
    data$location == '18019'|data$location =='18043'|data$location == '18061'|data$location == '18175' |data$location =='21029'|data$location == '21103'|data$location == '21111'|data$location == '21185' |data$location =='21211' |data$location =='21215'~"C.31140",
    data$location ==  '55021' |data$location =='55025'|data$location == '55045' |data$location =='55049' ~"C.31540",
    data$location == '48215' ~"C.32580",
    data$location == '05035'|data$location == '28033'|data$location == '28093'|data$location == '28137'|data$location =='28143' |data$location =='47047'|data$location == '47157' |data$location =='47167' ~"C.32820",
    data$location == '12011'|data$location == '12086' |data$location =='12099' ~"C.33100",
    data$location =='55079' |data$location =='55089' |data$location == '55131'|data$location == '55133' ~"C.33340",
    data$location == '27003'|data$location == '27019' |data$location =='27025' |data$location =='27037'|data$location == '27053'|data$location == '27059' |data$location =='27079' |data$location =='27095' |data$location =='27123' |data$location =='27139'|data$location == '27141'|data$location == '27163' |data$location =='27171'|data$location == '55093' |data$location =='55109' ~"C.33460",
    data$location == '06099' ~"C.33700",
    data$location =='47015'|data$location == '47021' |data$location == '47037' |data$location =='47043'|data$location == '47111'|data$location == '47119' |data$location =='47147' |data$location =='47149'|data$location == '47159'|data$location == '47165'|data$location == '47169' |data$location =='47187' |data$location =='47189'~"C.34980",
    data$location == '09009' ~"C.35300",
    data$location == '22051'|data$location == '22071'|data$location == '22075' |data$location =='22087'|data$location == '22089'|data$location == '22093' |data$location =='22095' |data$location =='22103' ~"C.35380",
    data$location == '36059'|data$location == '36103' |data$location =='34013' |data$location =='34019' |data$location =='34027'|data$location == '34037' |data$location =='34039' |data$location =='42103'|data$location == '34023'|data$location == '34025' |data$location =='34029'|data$location == '34035' |data$location =='34003' |data$location =='34017' |data$location =='34031' |data$location =='36005'|data$location == '36047' |data$location =='36061' |data$location =='36079' |data$location =='36081' |data$location =='36085' |data$location =='36087' |data$location =='36119'~"C.35620",
    data$location == '12081'|data$location == '12115' ~"C.35840",
    data$location == '49003'|data$location == '49011' |data$location =='49029'|data$location == '49057' ~"C.36260",
    data$location == '40017'|data$location =='40027' |data$location == '40051'|data$location == '40081'|data$location == '40083'|data$location == '40087' |data$location =='40109'~"C.36420",
    data$location == '19085'|data$location == '19129' |data$location == '19155'|data$location == '31025' |data$location =='31055' |data$location =='31153' |data$location =='31155'|data$location == '31177'~"C.36540",
    data$location == '12069' |data$location =='12095'|data$location == '12097' |data$location =='12117' ~"C.36740",
    data$location == '06111' ~"C.37100",
    data$location ==  '12009' ~"C.37340",
    data$location ==  '12033' |data$location =='12113' ~"C.37860",
    data$location == '34005'|data$location == '34007'|data$location == '34015'|data$location == '42017'|data$location == '42029'|data$location == '42091' |data$location =='42045' |data$location =='42101' |data$location =='10003' |data$location =='24015'|data$location == '34033'~"C.37980",
    data$location =='04013'|data$location == '04021'  ~"C.38060",
    data$location == '42003'|data$location == '42005' |data$location =='42007'|data$location == '42019'|data$location == '42051'|data$location == '42125' |data$location =='42129'~"C.38300",
    data$location == '23005'|data$location == '23023'|data$location == '23031' ~"C.38860",
    data$location == '41005'|data$location == '41009'|data$location == '41051'|data$location == '41067'|data$location == '41071' |data$location =='53011'|data$location == '53059'~"C.38900",
    data$location =='12085' |data$location == '12111' ~"C.38940",
    data$location == '36027'|data$location == '36071' ~"C.39100",
    data$location == '25005' |data$location =='44001'|data$location =='44003'|data$location == '44005' |data$location =='44007' |data$location =='44009'~"C.39300",
    data$location == '49023' |data$location == '49049'~"C.39340",
    data$location == '37069' |data$location == '37101'|data$location == '37183'~"C.39580",
    data$location == '51007'|data$location == '51036'|data$location == '51041' |data$location =='51053' |data$location =='51075'|data$location == '51085' |data$location =='51087' |data$location =='51097' |data$location =='51101'|data$location == '51127' |data$location =='51145'|data$location == '51149' |data$location =='51183'|data$location == '51570'|data$location == '51670'|data$location == '51730' |data$location =='51760'~"C.40060",
    data$location =='06065' |data$location == '06071' ~"C.40140",
    data$location == '36051'|data$location =='36055'|data$location == '36069'|data$location == '36073' |data$location =='36117'|data$location == '36123' ~"C.40380",
    data$location == '06017' |data$location =='06061' |data$location =='06067' |data$location =='06113'  ~"C.40900",
    data$location == '17005'|data$location == '17013' |data$location =='17027'|data$location == '17083'|data$location == '17117'|data$location == '17119' |data$location =='17133'|data$location == '17163'|data$location == '29071'|data$location == '29099' |data$location =='29113' |data$location =='29183' |data$location =='29189' |data$location =='29219' |data$location =='29510'~"C.41180",
    data$location == '49035'|data$location == '49045'~"C.41620",
    data$location == '48013'|data$location == '48019'|data$location == '48029' |data$location =='48091'|data$location == '48187' |data$location =='48259' |data$location =='48325'|data$location == '48493'~"C.41700",
    data$location ==  '06073'~"C.41740",
    data$location ==  '06001'|data$location == '06013'|data$location == '06075'|data$location == '06081' |data$location =='06041' ~"C.41860",
    data$location ==  '06069'|data$location == '06085'~"C.41940",
    data$location == '72007' |data$location == '72009' |data$location =='72017' |data$location =='72019'|data$location == '72021'|data$location == '72025'|data$location == '72029'|data$location == '72031'|data$location == '72033'|data$location == '72035' |data$location =='72037'|data$location == '72039' |data$location =='72041' |data$location =='72045' |data$location =='72047'|data$location =='72051' |data$location =='72053' |data$location =='72054'|data$location == '72061' |data$location =='72063' |data$location =='72069' |data$location =='72077'|data$location == '72085' |data$location =='72087' |data$location =='72089' |data$location =='72091' |data$location =='72095' |data$location =='72101' |data$location =='72103' |data$location =='72105' |data$location =='72107' |data$location =='72119' |data$location =='72127' |data$location =='72129' |data$location =='72135' |data$location =='72137' |data$location =='72139' |data$location =='72143' |data$location =='72145' |data$location =='72151'~"C.41980",
    data$location ==  '42069'|data$location == '42079'|data$location == '42131'~"C.42540",
    data$location == '53033'|data$location == '53061'|data$location == '53053'~"C.42660",
    data$location == '53063'|data$location == '53065'~"C.44060",
    data$location == '25011'|data$location == '25013'|data$location == '25015'~"C.44140",
    data$location == '06077' ~"C.44700",
    data$location =='36053' |data$location =='36067' |data$location =='36075' ~"C.45060",
    data$location == '12053' |data$location =='12057'|data$location == '12101'|data$location == '12103' ~"C.45300",
    data$location == '39051'|data$location == '39095'|data$location == '39123'|data$location == '39173' ~"C.45780",
    data$location == '04019' ~"C.46060",
    data$location == '40037'|data$location == '40111'|data$location == '40113' |data$location =='40117'|data$location == '40131'|data$location == '40143'|data$location == '40145'~"C.46140",
    data$location == '15003' ~"C.46520",
    data$location == '37029' |data$location =='37053' |data$location =='37073'|data$location == '51073'|data$location == '51093' |data$location =='51095'|data$location == '51115' |data$location =='51175'|data$location == '51199' |data$location =='51550' |data$location =='51620' |data$location =='51650'|data$location == '51700'|data$location == '51710'|data$location == '51735' |data$location =='51740'|data$location == '51800' |data$location =='51810'|data$location == '51830'~"C.47260",
    data$location == '24021'|data$location == '24031'|data$location == '11001' |data$location =='24009' |data$location =='24017'|data$location == '24033'|data$location == '51013'|data$location == '51043'|data$location == '51047'|data$location == '51059'|data$location == '51061'|data$location == '51107'|data$location == '51113' |data$location =='51153' |data$location =='51157'|data$location == '51177'|data$location == '51179'|data$location == '51187' |data$location =='51510' |data$location =='51600' |data$location =='51610' |data$location =='51630'|data$location == '51683'|data$location == '51685' |data$location =='54037'~"C.47900",
    data$location == '20015'|data$location == '20079'|data$location == '20173' |data$location =='20191' ~"C.48620",
    data$location == '37057'|data$location == '37059'|data$location == '37067' |data$location =='37169' |data$location =='37197'~"C.49180",
    data$location == '09015'|data$location ==  '25027' ~"C.49340",
    data$location == '39099'|data$location == '39155' |data$location == '42085'~"C.49660")
  
  data <- data %>%
    rename(value_original = value) %>%
    filter(!is.na(msa_estimate)) #Remove counties that are not part of MSAs#
  
  if(grepl("allcounty_08.21_total", filename)) {
    data <- data %>%
      group_by(msa_estimate) %>%
      mutate(value = sum(value_original)) %>%
      ungroup()
  }
  if(grepl("allcounty_08.21_risk", filename)) {
    data <- data %>%
      group_by(msa_estimate, risk) %>%
      mutate(value = sum(value_original)) %>%
      ungroup()
  }
  if(grepl("allcounty_08.21_sex", filename)) {
    data <- data %>%
      group_by(msa_estimate, sex) %>%
      mutate(value = sum(value_original))
  }
  if(grepl("allcounty_14.21_race", filename)) {
    data <- data %>%
      group_by(msa_estimate, race) %>%
      mutate(value = sum(value_original))
  }
  if(grepl("allcounty_08.21_age", filename)) {
    data <- data %>%
      group_by(msa_estimate, age) %>%
      mutate(value = sum(value_original)) %>%
      ungroup()
  }
  
  data= as.data.frame(data)
  list(filename, data)
})

################################################################################
#Create County totals of prevalence to estimate MSAs for 2017 only#
################################################################################
prevalence_msa_est = lapply(data.list.msa.estprev, `[[`, 2) 

for (data in prevalence_msa_est) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Atlas Plus data')
}
