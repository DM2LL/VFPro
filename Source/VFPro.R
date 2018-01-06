#' This function identifies Eye Progression using GEM Axes & Critical P-Value/Slope
#' @param
#' @EyeData
#' VFPro()

# Date Created: October 07, 2017
# Version: Beta version
# Parameter:
# Eye Data: csv file

# Validations:
# File Format - csv file format
# Eye Data - should have minimum of 5 visits


exit <- function() {
  rm(list = ls(all = TRUE))
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}

checkRequiredPackage <- function(x) {
  for (i in x) {
    if (!require(i, character.only = TRUE))
    {
      install.packages(i, dep = TRUE)
      if (!require(i, character.only = TRUE))
        stop("Package not found")
    }
  }
}

validateparameters <- function (EyeData) {
    # check if input file exists and in csv format
    
    if (!file.exists(EyeData)) {
      print (paste('Missing File:', EyeData))
      exit()
    }
    # check Eye Data file
    # column 1 = eyeID
    # column 2 = Age (can also be date - leave column name as Age)
    # column 3 to 5
    eye_data = read.csv(EyeData, header = TRUE)
    if (nrow(eye_data) < 3 || ncol(eye_data) < 54) {
      print (paste(EyeData, 'should have 54 columns and at least 2 rows'))
      exit()
    }
    if ("IDEYE" %in% toupper(colnames(eye_data)) == FALSE) {
      print (paste('Column ideye is missing'))
      exit()
    }
    if ("AGE" %in% toupper(colnames(eye_data)) == FALSE) {
      print (paste('Column Age is missing'))
      exit()
    }
    if ("TD_" %in% substr(toupper(colnames(eye_data)), 1, 3) == FALSE) {
      print (paste('Column TD_ is missing'))
      exit()
    }
    
    eye_data_below5visitsn = NULL
    eye_data_below5visits = NULL
    # check for eye with less than 5 visits
    eye_data = arrange(eye_data, eye_data$IdEye, eye_data$Age) # make sure that visits are sorted
    eye_dataid = unique(eye_data[, 1])
    eye_data_transpose <- as.matrix(table(eye_data$IdEye))
    for (e in 1:nrow(eye_data_transpose)) {
      if (eye_data_transpose[e, ] < 5) {eye_data_below5visits <-rbind(eye_data_below5visits, eye_data_transpose[e, ])
      eye_data_below5visitsn <-rbind(eye_data_below5visitsn, rownames(eye_data_transpose)[e])
      }
    }
}  


VFPro <- function (EyeData) {

  require(csv)
  require(data.table)
  require(plyr)
  require(tools)  

criticalpvalue_var = 0.066861627

print ("Cheking required R Packages...")
checkRequiredPackage(c("csv", "data.table", "plyr", "tools"))
print ("Validating parameters...")
validateparameters(EyeData)


# GEM Axes Data
print ("Creating GEM Axes...")
    gem_data = matrix(c(-0.28764586,-0.15496814,-0.14122740,0.12142820,0.36762564,0.06319578,0.21139305,-0.33039440,0.11534901,-0.18744982,-0.15067214,0.11951552,-0.13574150,0.17522593,0.04298499,-0.01363404,-0.16985847,-0.07138208,0.03254832,-0.09048659,
                        -0.26110943,-0.21912311,-0.19317422,0.19362656,0.26672375,0.18585383,-0.53994797,-0.23519045,0.08561322,0.18017100,-0.14761826,0.10542282,-0.16604873,0.15895044,0.06645652,0.01242656,-0.17389310,-0.12174246,0.03177924,-0.01976724,
                        -0.25754144,-0.30203600,-0.16399318,0.23205654,-0.07398447,-0.15765426,-0.15046115,0.21196156,0.14930238,-0.05448436,-0.13911080,0.10753658,-0.22971438,0.08309072,0.11943946,-0.13168699,-0.19653568,0.07509597,-0.20488049,-0.19346060,
                        -0.31810991,-0.32463487,-0.04533320,0.27192937,-0.25071908,-0.18166828,0.12807451,0.40264006,-0.35488569,-0.22212429,-0.12278127,0.07590413,-0.30307675,0.04377648,0.16207095,-0.16985914,-0.13703867,0.02961210,-0.19688125,-0.00531906,
                        -0.19383108,-0.05174773,-0.01576541,-0.19451574,0.20666600,0.24984519,0.13298926,-0.00729203,-0.34280811,0.13805982,-0.17095688,0.13224591,-0.06446303,0.16389577,-0.02483130,0.08232680,-0.18128778,-0.13675352,0.10491956,0.04585601,
                        -0.14521289,-0.06213168,0.02838929,-0.07669352,0.11003915,0.19448775,0.00594935,-0.03980095,-0.04011613,0.06870914,-0.17340679,0.13178359,-0.01099729,0.09240046,0.06385981,0.17387428,-0.07851465,-0.14901558,0.15909310,-0.05030430,
                        -0.15945581,-0.09913254,0.01524672,-0.04478820,0.04384600,0.13498408,-0.11570287,-0.06571719,-0.07261890,-0.01144860,-0.17358928,0.14594920,-0.04757329,0.04792121,0.08203515,0.09680652,-0.11752891,-0.09245075,0.10187698,-0.08698869,
                        -0.16923833,-0.15500335,0.01078180,-0.01184841,-0.11229365,0.03928309,-0.03518471,0.09953772,0.02737901,0.01003329,-0.15491610,0.14496773,-0.12853503,-0.03460615,0.18552611,0.06646405,-0.03873120,0.08351203,-0.05291182,-0.01032620,
                        -0.13435424,-0.11057143,0.05977169,-0.03679338,-0.16742186,0.08198916,0.12979161,0.08727292,0.04888920,0.07087124,-0.13304894,0.08910761,-0.16834039,-0.09987037,0.22656803,0.07812957,0.05426851,0.10179372,-0.04308127,0.09249961,
                        -0.17044352,-0.12362278,0.09913164,-0.00856085,-0.25681968,0.02679844,0.28977024,-0.14088496,0.37821381,0.30039417,-0.11380550,0.08693844,-0.27412889,-0.04507197,0.16026598,-0.09236272,0.03998874,0.07369358,-0.02729011,0.14730852,
                        -0.18609734,0.03456262,-0.17165500,-0.37113972,0.17192478,0.00523827,0.28466609,0.07078758,-0.25058270,-0.06542577,-0.17777616,0.14532980,0.03344888,0.20026942,-0.16600914,-0.04353658,0.01723414,-0.16116964,-0.05407026,0.06785288,
                        -0.10826941,0.01108691,0.00521554,-0.14752327,0.05845419,0.12207751,0.04452211,0.02967207,-0.07541233,0.02736953,-0.17996799,0.15648628,0.11145981,0.08896415,-0.01807043,0.12759363,0.01142397,-0.20658397,0.20655706,0.02502155,
                        -0.09947564,-0.00657822,0.06683586,-0.12307837,0.03191409,0.14742932,-0.02306952,0.04094323,-0.06253317,0.01911404,-0.17603525,0.15276766,0.08980528,-0.00176705,0.05283511,0.17707600,0.01580670,-0.11451186,0.24666792,-0.10139170,
                        -0.10016575,-0.02808131,0.04320822,-0.13420197,-0.00070633,0.11115438,-0.08233460,0.02604900,-0.08321124,-0.03134052,-0.18049866,0.16349625,0.13826398,-0.11345459,-0.01808736,0.11753278,0.10258942,0.13620568,0.04491841,-0.15986125,
                        -0.10370522,-0.05189294,0.07840497,-0.08680610,-0.07786337,0.08069626,-0.06202553,0.03006406,0.00133530,-0.01439462,-0.15971572,0.15807806,0.09771875,-0.22725184,0.09262862,0.08147711,0.16649373,0.38619772,-0.15578180,-0.01704064,
                        -0.09343689,-0.05167663,0.08088967,-0.10118056,-0.11082336,0.12918136,-0.04723923,0.08725470,0.04082685,0.06731200,-0.15608027,0.15102107,0.01187058,-0.16538184,0.18330287,0.24372625,0.18063454,0.16483534,-0.10372540,0.00561597,
                        -0.12836858,-0.05323864,0.08436870,-0.09492904,-0.17245917,0.14663950,0.12862828,-0.00280152,0.13920100,0.16466694,-0.12316890,0.08331345,-0.16626036,-0.11922378,0.15023413,-0.03178119,0.12471964,0.05124465,0.02651639,0.11609140,
                        -0.17291997,-0.07806996,0.06118720,-0.01390875,-0.14581961,-0.03672682,0.24876423,-0.18510680,0.15507184,0.07468136,-0.07866742,0.03018101,-0.22485038,-0.04259212,0.05430046,-0.15555617,0.16103190,0.08335435,0.11910615,0.13447390,
                        -0.19871482,0.20973144,-0.55685240,-0.39859957,-0.12138825,-0.35059370,-0.19266838,0.06683270,0.28049994,-0.02054088,-0.15504327,0.12461769,0.10804523,0.22202810,-0.19899321,-0.13188216,0.18369072,-0.06894681,-0.37536699,0.28231835,
                        -0.15476758,0.12597829,-0.16388147,-0.23723988,-0.00978322,-0.04649535,0.07248599,0.05523197,-0.00636349,0.05109604,-0.16687161,0.12105219,0.12910261,0.21522570,-0.17657048,-0.10821425,0.16517811,-0.13301321,-0.23131962,0.17308687,
                        -0.10157389,0.04685875,0.04108779,-0.16315642,0.02079049,0.03811513,0.01963770,0.05774135,0.00066969,-0.06350358,-0.18212136,0.14510782,0.19185314,0.03922738,-0.09540519,0.05080909,0.13135199,-0.11410637,0.06229244,0.07340861,
                        -0.07757327,0.03452634,0.06812487,-0.12816088,-0.00507769,0.08863187,-0.07807716,0.02295086,-0.04407514,-0.08577260,-0.17596383,0.17814343,0.24945192,-0.06727190,-0.09816502,0.01964347,0.11743307,0.07182959,0.02156845,-0.08010076,
                        -0.08297987,0.02308320,0.08717721,-0.09825808,-0.05331366,0.09493419,-0.11009573,-0.03085468,-0.03027505,-0.08528869,-0.13612177,0.15199312,0.28626894,-0.19466817,-0.18190421,-0.18184667,-0.09154402,0.28282981,-0.07089156,-0.09492791,
                        -0.07843672,0.01887407,0.10075599,-0.04843664,-0.05969568,0.04455882,-0.12424841,-0.00746997,0.00270528,-0.10809933,-0.09094987,0.08016598,0.12076848,-0.18329589,-0.09383880,-0.34416315,-0.41518428,0.02459711,0.12464278,-0.01225180,
                        -0.07771196,-0.01190037,0.10135887,-0.08524599,-0.07977964,0.04684828,-0.09982759,0.01171092,0.02120238,-0.02000856,-0.11689017,0.10794798,0.09637764,-0.22633701,-0.10638346,-0.32246289,-0.33028108,0.06236766,0.16878069,0.06191815,
                        -0.14019695,-0.02652011,0.07526196,0.02189730,-0.17814726,-0.07411193,0.09119986,-0.35297508,0.04016347,-0.03531748,-0.06599564,0.01158899,-0.17833108,-0.05458108,-0.01748932,-0.17612084,0.19559702,0.04052600,0.15144415,0.16100409,
                        -0.16755785,0.50963764,-0.33207857,0.38799874,-0.35257508,0.26336081,-0.01338914,-0.13397108,-0.32735060,0.18676071,-0.14592595,-0.14501591,0.02438629,0.29687598,0.00267368,-0.24302063,0.15541437,0.18222841,0.00640453,-0.26222088,
                        -0.12067989,0.21668366,-0.04763761,0.10773236,-0.02875966,0.07342668,0.09474090,0.02038473,0.07403111,-0.05173585,-0.15554405,-0.16140244,0.07031019,0.26231848,0.10679625,-0.22751237,0.13888694,0.12616593,0.00436491,-0.30288641,
                        -0.07799073,0.11545786,0.07020272,-0.01421153,0.00344662,0.04480671,-0.04294574,0.04234345,0.10740790,-0.11616623,-0.15746719,-0.19477544,0.13666045,0.06232746,0.18119871,-0.09532332,0.06729002,-0.04429308,-0.00910874,-0.11641941,
                        -0.06674924,0.08886416,0.10366116,-0.03018584,-0.02207971,0.05743311,-0.10279886,0.03666022,0.01670310,-0.07949776,-0.14301311,-0.21046757,0.16571284,-0.03717095,0.23052723,-0.11438082,-0.00730363,-0.12388190,0.00104387,0.03945393,
                        -0.06673800,0.06672408,0.10791177,-0.06149022,-0.05981581,0.02846674,-0.12441831,0.00555651,-0.00259589,-0.06575329,-0.08517420,-0.09816360,0.06580888,-0.11446084,0.08410647,-0.15926813,-0.07723407,-0.07747135,-0.14418838,0.23097805,
                        -0.06527377,0.03115199,0.11497577,-0.05390204,-0.06274625,0.03044043,-0.12217378,0.01904348,0.02776877,-0.08165440,-0.03713843,-0.02428562,-0.00679015,-0.11039366,-0.00812209,-0.10318634,0.03163023,-0.02716555,0.06544894,0.19342945,
                        -0.07917114,0.03573990,0.11762714,-0.05144045,-0.08189570,0.03462060,-0.10820323,0.02491716,0.06294727,-0.05563396,-0.06179856,-0.04381089,-0.01753248,-0.11896282,-0.01658660,-0.12861244,0.07831875,-0.05553165,0.13191479,0.20846489,
                        -0.08003765,0.00682428,0.11250556,-0.07544299,-0.18379222,-0.23366724,-0.08318295,-0.47632814,-0.21149258,-0.49884979,-0.05745192,-0.02795724,-0.14080937,-0.06515135,-0.07726533,-0.16134379,0.23281534,-0.08233907,0.23850239,0.14976706,
                        -0.16288508,0.27669832,0.01288819,0.22264922,0.16384982,0.17683190,0.23758680,0.11677601,0.35649723,-0.40847973,-0.13737573,-0.21360772,-0.00752001,0.19257139,-0.02394750,-0.04634884,0.01080979,0.25739791,0.19690238,0.04843229,
                        -0.08148374,0.12236576,0.09933358,0.04141239,0.00683322,0.07535666,-0.05284519,0.08896405,0.07571161,-0.04695476,-0.14562962,-0.22789255,0.10383092,0.09624386,0.12145380,0.03540047,-0.02819271,0.12432646,0.17164169,0.01751019,
                        -0.06593444,0.12058655,0.11505723,-0.00783861,-0.01698932,0.04869353,-0.06283011,0.11234742,0.03207991,-0.03179945,-0.15496169,-0.21842817,0.13395386,0.00653995,0.17489658,0.07700994,-0.01753200,-0.05001801,0.02963020,0.08852966,
                        -0.06800583,0.10099549,0.12655412,-0.04029353,-0.02799634,-0.00358278,-0.09426636,0.05108022,0.02330225,-0.05700978,-0.15215419,-0.20985983,0.12787431,-0.07209002,0.22210638,-0.03209357,-0.03048327,-0.19742430,-0.18060094,-0.01740469,
                        -0.06614406,0.05060918,0.14281574,-0.05735802,-0.06337258,0.00975213,-0.09289024,0.02065980,0.00289901,-0.04793920,-0.13357880,-0.14776165,0.07912200,-0.18168434,0.11739723,-0.04643424,-0.05275585,-0.23215133,-0.16979161,0.09091588,
                        -0.06998094,0.05205915,0.14264978,-0.06288508,-0.06790102,0.03539395,-0.09567315,0.04271683,0.02054607,0.01598173,-0.12586578,-0.16792782,0.05058526,-0.17015915,0.11004611,0.01365135,0.04414300,-0.21166844,-0.09215071,-0.12004743,
                        -0.08083188,0.03801320,0.12760914,-0.03752058,-0.05597927,-0.05272716,-0.07354737,-0.03526179,-0.02775666,0.03876340,-0.09997156,-0.08768132,-0.09651714,-0.19673558,-0.08957442,-0.03153014,0.15268235,-0.12522036,0.13702737,-0.20353615,
                        -0.09657858,0.05941906,0.12330462,0.01548706,-0.00843710,-0.13696641,0.03669177,-0.21940925,-0.09176688,-0.01266927,-0.08500683,-0.03009008,-0.15858977,-0.09822539,-0.14620100,-0.14962022,0.26865050,-0.13696283,0.17572922,-0.01526084,
                        -0.11496105,0.19214339,0.12621124,0.09018752,0.16160114,0.06384636,-0.01789502,0.16634510,0.11425272,-0.03907205,-0.14252863,-0.18358353,-0.02852748,0.09972048,-0.13740032,0.12868855,-0.11784313,0.22173497,0.17212392,0.12622873,
                        -0.07520988,0.10519852,0.12155560,0.02100413,0.08793626,-0.02616852,-0.03093757,0.08856674,0.02342183,-0.00380544,-0.14339755,-0.18573409,0.01008137,0.04149672,-0.06284349,0.17242812,-0.08178878,0.17432836,0.14131815,0.22612407,
                        -0.08912054,0.10653970,0.12369247,0.02308249,0.06417444,-0.04066228,-0.07571265,0.07957933,0.02426104,-0.00382097,-0.14647540,-0.17147003,0.00021975,-0.03689794,-0.06660977,0.23914296,-0.11867287,0.10101418,-0.07389243,0.19123034,
                        -0.08347562,0.07633874,0.14880233,-0.01782644,0.02277534,-0.06697413,-0.10946057,0.02302050,-0.01851822,0.04674166,-0.14023875,-0.13949394,-0.03529228,-0.16289942,-0.10865686,0.14151675,-0.10184631,-0.04589044,-0.12059189,0.07085464,
                        -0.07715479,0.05615776,0.12102679,-0.00889699,-0.03763828,-0.03560224,-0.06703567,0.02430962,0.01362907,0.04612509,-0.12780373,-0.11175769,-0.07613534,-0.20542892,-0.16692594,0.08921617,-0.00843845,-0.15392567,-0.05267536,-0.16988930,
                        -0.11082351,0.03901638,0.16650898,-0.01676726,-0.01361019,-0.11086686,-0.03277169,0.00052534,-0.01723357,0.18149684,-0.11571221,-0.07513179,-0.13580982,-0.16129129,-0.16764537,-0.00158808,0.10005613,-0.08787279,0.08039142,-0.21435517,
                        -0.17372564,0.13898945,0.12307854,0.13626065,0.32950469,-0.35484161,0.13351715,0.05919745,-0.08583276,0.19160143,-0.13577524,-0.15874770,-0.12033208,0.08388174,-0.21025353,0.06955834,-0.11063179,0.13441204,-0.05860867,0.12445209,
                        -0.13028923,0.11391894,0.10052833,0.06244990,0.17451634,-0.24651848,-0.00904856,0.04217097,-0.06702833,0.05323307,-0.13065430,-0.13791261,-0.14571451,0.02777470,-0.24934870,0.16925962,-0.07169369,0.05969119,-0.13045444,0.01491568,
                        -0.12733256,0.08288089,0.11558098,0.04441563,0.07908237,-0.25797504,-0.10830162,-0.01457006,-0.00828607,0.17122857,-0.12194144,-0.10239927,-0.16415275,-0.08819921,-0.25261786,0.03987748,-0.01443071,0.01703683,-0.19906290,-0.21189255,
                        -0.11767325,0.10885785,0.14971902,0.02643217,-0.00965433,-0.22472949,0.02499154,-0.10350847,-0.08719007,0.27652064,-0.10311768,-0.09436471,-0.18855104,-0.09919722,-0.24911661,0.02529653,0.03182291,-0.06176954,-0.09414664,-0.16309905	),
                      nrow=20,ncol=52)

#create csv for users to view the GEM data
print ("Writing GEM axes to csv file...")
write.table(gem_data,"gem.csv",col.names = FALSE,sep = ",",row.names = FALSE)
print ("Calculating... Please wait...")
dt = NULL
# Read Eye Data
eye_data = read.csv(EyeData, header = TRUE)
eye_data = arrange(eye_data, eye_data$IdEye, eye_data$Age) # make sure that visits are sorted
eye_dataid = unique(eye_data[, 1]) # get distinct list of eye id from the sample. This will be used later to report if eye progressed

# Compute for slope and p-value

for (i in eye_dataid) {  # loop through all unique eye id
  
  current_eyedata <- eye_data[which(eye_data$IdEye == i),] # current eye id
  x = as.matrix(current_eyedata[, 3:54])
  eyeid = i
  age = as.character(current_eyedata[, 2])
  
  for (j in c(1:20)) { # loop GEM Axes (20 rows)
    gem_axes = j
    y = gem_data[gem_axes, ]
    result.temp = NULL
    
    # matrix multiplication
    mm = x %*% y 
    
    # linear regression
    lm.result = summary(lm(age ~ mm))
    pValue = lm.result$coefficients[2,4]
    slope = lm.result$coefficients[2,1]
    criticalPValue = criticalpvalue_var
    criticalSlope = 0
    if ((as.numeric(pValue) < as.numeric(criticalPValue)) && (as.numeric(slope) < as.numeric(criticalSlope))) {
      result_progress = "Progressed"
    } else {
      result_progress = "Not Progressed"
    }
    dt = rbind(dt,data.table(gem_axes,eyeid,pValue,slope,criticalPValue,criticalSlope,result_progress))
  }
}
print ("Writing detail result to csv file...")
write.csv(dt, file = "resultdetail.csv")
 
progressedResult = NULL

# extract progressed eye
print ("Filtering Progressed eyes...")
progressedResult_extract <- dt[which(dt$result_progress == 'Progressed'),]
print ("Filtering Progressed eyes complete...")
print ("Getting date when progression first detected...")
# look for eyes with 3 progressed
print ("Counting progressed result from individual GEM Axes...")
rows.per.group  <- aggregate(rep(1, length(paste0(progressedResult_extract$eyeid))),
                             by=list(progressedResult_extract$eyeid), sum)
print ("Selecting which ones have 3 progressions")
progressedEyeList  <- unique(rows.per.group[which(rows.per.group$x >= 3),][,1])
print ("Extracting GEM Axis used to identify progression...")
progressedEyeListGEMaxes <- progressedResult_extract[which(progressedResult_extract$eyeid %in% progressedEyeList),]
print ("Sorting Eye and Gem Axis...")
progressedEyeListGEMaxesOrdered <- progressedEyeListGEMaxes[order(progressedEyeListGEMaxes$eyeid,progressedEyeListGEMaxes$gem_axes),]
print ("Extracting Unique Eye and Gem Axis...")
progressedEyeListGEMaxesUnique <- progressedEyeListGEMaxesOrdered[!duplicated(progressedEyeListGEMaxesOrdered$eyeid),]
print ("Getting date for progressed eye...")
# getting the date when progression was first detected
progressedResult_top = NULL
lookingforprogressiondated = TRUE
row_age = 5
for (p in progressedEyeList) {
  while (lookingforprogressiondated) {
      
      current_progressedeyedata <- head(eye_data[which(eye_data$IdEye == p),],row_age) # current eye id
      px = as.matrix(current_progressedeyedata[, 3:54])
      progressed_eyeid = p
      age = as.character(current_progressedeyedata[, 2])
      last_age = as.character(current_progressedeyedata[row_age, 2])
      
      #print (paste("Getting gem_axes for eye", p))
      pj<- progressedEyeListGEMaxesUnique[which(progressedEyeListGEMaxesUnique$eyeid == p),]
      
      gem_axes <- pj$gem_axes
      
        py = gem_data[gem_axes, ]
        result.temp = NULL
        # matrix multiplication
        mm = px %*% py 
        # linear regression
        lm.result = summary(lm(age ~ mm))
        
        if (is.nan(lm.result$coefficients[2, 4])) {
            result.temp = c(result.temp, c(1.0, lm.result$coefficients[2, 1]))
          } else {
            result.temp = c(result.temp,c(lm.result$coefficients[2, 4] * 0.5, lm.result$coefficients[2, 1]))
          }
          pValue = result.temp[1]
          slope = result.temp[2]
          criticalPValue = criticalpvalue_var
          criticalSlope = 0
          
          if ((as.numeric(pValue) < as.numeric(criticalPValue)) && (as.numeric(slope) < as.numeric(criticalSlope))) {
              progressedResult_top = rbind(progressedResult_top,data.table(progressed_eyeid,last_age))
              lookingforprogressiondated = FALSE
              row_age = 5
              break
          } else {
            row_age = row_age + 1
          }
       }
      
  lookingforprogressiondated = TRUE
  row_age = 5
  }


# create vector with 
# column 1: 0 - not progress and 1 
# column 2: age first observed progression, age last visit if eye not progressed

# get eyes last visit
print ("Getting last visit for each eye...")
Eye_LastVisit = NULL
Eye_LastVisit <- aggregate(eye_data$Age, by = list(eye_data$IdEye), max)
names(Eye_LastVisit) <- c("IdEye","Age")
print ("Getting first visit for each eye...")
Eye_FirstVisit = NULL
Eye_FirstVisit <- aggregate(eye_data$Age, by = list(eye_data$IdEye), min)
names(Eye_FirstVisit) <- c("IdEye","Age")


VectorAllEyes = NULL

for (eye_ID in eye_dataid) {
  last_visit = NULL
  Progressed = NULL
  if (eye_ID %in% progressedResult_top$progressed_eyeid) {
    Progressed = "1"
    
    first_visit = as.numeric(Eye_FirstVisit[which(Eye_FirstVisit$IdEye == eye_ID),2])
    last_visit = as.numeric(progressedResult_top[which(progressedResult_top$progressed_eyeid == eye_ID),2])
    observed <- last_visit - first_visit
    VectorAllEyes <- rbind(VectorAllEyes,data.table(Progressed,observed,eye_ID))
  } else
  {
    Progressed = "0"
    
    first_visit = as.numeric(Eye_FirstVisit[which(Eye_FirstVisit$IdEye == eye_ID),2])
    last_visit = as.numeric(Eye_LastVisit[which(Eye_LastVisit$IdEye == eye_ID),2])
    observed <- last_visit - first_visit
    VectorAllEyes <- rbind(VectorAllEyes,data.table(Progressed,observed,eye_ID))
  }
}

write.table(VectorAllEyes,"vectorAllEyes.csv",col.names = TRUE,sep = ",",row.names = FALSE)



if (is.data.frame(progressedResult_extract) && nrow(progressedResult_extract) != 0) {

  print ("Writing Progressed Eye data to csv file...")
  write.table(progressedResult_top,"progressedresult.csv",col.names = TRUE,sep = ",",row.names = FALSE)
  return (progressedResult_top)
  #View (progressedResult_top)
  
} else { 
  print ("No progression detected...")
  write.table(progressedResult_top,"progressedresult.csv",col.names = TRUE,sep = ",",row.names = FALSE)
  return (progressedResult_top)
  #View (progressedResult_top) 
}



print ("Process complete...")

}
