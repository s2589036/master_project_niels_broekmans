#=====================================================================================================================================
#=============================================================LOAD PACKAGES===========================================================
#=====================================================================================================================================


dev.off()
#install.packages("spacyr", INSTALL_opts = '--no-lock')
#install.packages("mgcv")
#install.packages("itsadug")
#install.packages("randomcoloR")
#install.packages("factoextra")
library(plyr)
library(stringr)
library(qdap)
library(spacyr)
library(mgcv)
library(itsadug)
library(ggplot2)
library(randomcoloR)
library(ggpubr)
library(tidyr)
library(factoextra)
library(dplyr)



setwd("G:/Mijn Drive/Studie informatiekunde/master/master project/project")

My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.title.y = element_text(size = 16))
#=========================================================================================================================================
#=========================================================================================================================================
#=========================================================================================================================================
#===============================================================IDIOM ANALYSIS============================================================
#=========================================================================================================================================
#=========================================================================================================================================
#=========================================================================================================================================


#=====================================================================================================================================
#=============================================================IMPORT IDIOMS===========================================================
#=====================================================================================================================================
datalist = list()

#12,15,74 verb added (N=3)
#1, 31,127,129 removed (1: regen in de drup: problem with corpus; rest too literal )
for (i in(1:130)[c(-1,-12,-15,-31,-74,-127,-129)]) {
  dat <- read.csv(paste("idioms_sonar\\after_changes\\without_verb\\",i,".csv",sep=""),encoding="UTF-8")
  dat$with_verb <- 0
  dat$idiom_id <- i  # maybe you want to keep track of which iteration produced it?
  dat$verb <- ""
  datalist[[i]] <- dat # add it to your list
}

allidiomstype1 = do.call(rbind, datalist)


#==========================================================================================================
#===========COMBINE QUERY FILE OF STATIC PART WITH A SEARCH FOR VERB FORM IN CONTEXTS======================
#==========================================================================================================

findidioms <- function(i,verbforms){
  verbformspattern <- paste("\\b",paste(verbforms, collapse = "\\b|\\b"),"\\b",sep="")
  filename <- paste("idioms_sonar\\after_changes\\with_verb\\",i,".csv",sep="")
  idiomstatic <- read.csv(filename,encoding="UTF-8")
  idiomstatic <- idiomstatic[ with(idiomstatic,  grepl(verbformspattern, left_context)  | grepl(verbformspattern, right_context)  ) , ]
  idiomstatic$with_verb <- 1
  idiomstatic$idiom_id <- i
  idiomstatic$verb <- verbforms[1] #<- add this in order to put the verb in crosstable as well (concatenated to static part)
  idiomstatic
}

#183,186 removed
allidiomstype2 <- data.frame()
allidiomstype2 <- rbind(allidiomstype2, findidioms(12,c("dragen","draag","draagt","droeg","droegen", "gedragen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(15,c("gaan","ga","gaat","ging","gingen","gegaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(74,c("lopen","loop","loopt","loopte","loopten","gelopen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(131,c("houden","houd","hou","houdt","hield","hielden","gehouden")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(132,c("halen","haal","haalt","haalde","haalden","gehaald")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(133,c("blijven","blijf","blijft","bleef","bleven","gebleven")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(134,c("doen","doe","doet","deed","deden","gedaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(135,c("draaien","draai","draait","draaide","draaiden","gedraaid")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(136,c("grijpen","grijp","grijptt","greep","grepen","gegrepen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(137,c("houden","houd","hou","houdt","hield","hielden","gehouden")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(138,c("helpen","help","helpt","hielp","hielpen","geholpen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(139,c("kloppen","klop","klopt","klopte","klopten","geklopt")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(140,c("knopen","knoop","knoopt","knoopte","knoopten","geknoopt")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(141,c("komen","kom","komt","kwam","kwamen","gekomen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(142,c("leven","leef","leeft","leefde","leefden","geleefd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(143,c("leren","leer","leert","leerde","leerden","geleerd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(144,c("lichten","licht","lichtte","lichtten","gelicht")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(145,c("lopen","loop","loopt","loopte","loopten","gelopen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(146,c("lopen","loop","loopt","loopte","loopten","gelopen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(147,c("nemen","neem","neemt","nam","namen","genomen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(148,c("roeien","roei","roeit","roeide","roeiden","geroeid","oproeien","oproei","oproeit","oproeit")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(149,c("schreeuwen","schreeuw","schreeuwt","schreeuwde","schreeuwden","geschreeuwd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(150,c("spelen","speel","speelt","speelde","speelden","gespeeld")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(151,c("spugen","spuug","spuugt","spuugde","spuugden","spoog","spogen","gespuugd",
                                                         "spuwen","spuw","spuwt","spuwde","spuwden","gespuwd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(152,c("staan","sta","staat","stond","stondden","gestaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(153,c("strijken","strijk","strijkt","streek","streken","gestreken")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(154,c("toveren","tover","tovert","toverde","toverden","getoverd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(155,c("trekken","trek","trekt","trok","trokken","getrokken")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(156,c("trekken","trek","trekt","trok","trokken","getrokken")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(157,c("vallen","val","valt","viel","vielen","gevallen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(158,c("vallen","val","valt","viel","vielen","gevallen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(159,c("vechten","vecht","vocht","vochten","gevochten")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(160,c("wippen","wip","wipt","wipte","wipten","gewipt")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(161,c("zitten","zit","zat","zaten","gezeten")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(162,c("zetten","zet","zette","gezet")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(163,c("zweten","zweet","zweette","zweetten","gezweet")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(164,c("kijken","kijk","kijkt","keek","keken","gekeken")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(165,c("spelen","speel","speelt","speelde","speelden","gespeeld")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(166,c("vatten","vat","vatte","gevat")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(167,c("trappen","trap","trapt","trapte","trapten","getrapt")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(168,c("trouwen","trouw","trouwt","trouwde","trouwden","getrouwd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(169,c("zitten","zit","zat","zaten","gezeten")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(170,c("krijgen","krijg","krijgt","kreeg","kregen","gekregen")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(171,c("brengen","breng","brengt","bracht","brachten","gebracht")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(172,c("doen","doe","doet","deed","deden","gedaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(173,c("hebben","heb","hebt","heeft","had","hadden","gehad")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(174,c("houden","houd","hou","houdt","hield","hielden","gehouden")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(175,c("houden","houd","hou","houdt","hield","hielden","gehouden",
                                                         "hebben","heb","hebt","heeft","had","hadden","gehad")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(176,c("houden","houd","hou","houdt","hield","hielden","gehouden",
                                                         "hebben","heb","hebt","heeft","had","hadden","gehad")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(177,c("kloppen","klop","klopt","klopte","klopten","geklopt")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(178,c("eten","eet","at","aten","gegeten")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(179,c("slaan","sla","slaat","sloeg","sloegen","geslaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(180,c("staan","sta","staat","stond","stondden","gestaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(181,c("staan","sta","staat","stond","stondden","gestaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(182,c("stoten","stoot","stootte","stootten","gestoten")))
#allidiomstype2 <- rbind(allidiomstype2, findidioms(183,c("gaan","ga","gaat","ging","gingen","gegaan")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(184,c("vertrouwen","vertrouw","vertrouwt","vertrouwde","vertrouwden","vertrouwd",
                                                         "toevertrouwen","toevertrouw","toevertrouwt","toevertrouwde","toevertrouwden","toevertrouwd")))
allidiomstype2 <- rbind(allidiomstype2, findidioms(185,c("maken","maak","maakt","maakte","maakten","gemaakt")))
#allidiomstype2 <- rbind(allidiomstype2, findidioms(186,c("lopen","loop","loopt","liep","liepen","gelopen")))

idioms <- rbind(allidiomstype1,allidiomstype2)
#this function works but the annotation of pos_head is often wrong
idioms$amountofnouns <- str_count(idioms$pos_head,"N")-str_count(idioms$pos_head,"VNW")

#remove empty columns
idioms <- idioms[c(1,2,3,4,5,6,7,8,32,35,36,37,38)]

#add id
idioms$id <- seq.int(nrow(idioms))

#add column names
colnames(idioms) <- c("doc_id", "doc_name","left_context","idiom_found","right_context","idiom_lemma","pos","pos_head","xml_id","with_verb","idiom_id","verb","amountofnouns","id")

#add doc_type
idioms$doc_type <- substr(idioms$doc_id,1,8)

#add doc_type_name: doc_type with a meaningful name: e-newsletters (WR-P-P-E) and and 
idioms$doc_type_name <- revalue(idioms$doc_type, c("WR-P-E-A"="discussion lists","WR-P-E-C"="e-magazines","WR-P-E-E"="E-newsletters","WR-P-E-F"="press releases","WR-P-E-G"="subtitles","WR-P-E-H"="teletext pages","WR-P-E-I"="websites","WR-P-E-J"="wikipedia","WR-P-E-K"="blogs","WR-P-E-L"="tweets","WR-P-P-B"="books","WR-P-P-C"="brochures","WR-P-P-D"="newsletters","WR-P-P-E"="guides manuals","WR-P-P-F"="legal texts","WR-P-P-G"="newspapers","WR-P-P-H"="periodicals magazines","WR-P-P-I"="policy documents","WR-P-P-J"="proceedings","WR-P-P-K"="reports","WR-U-E-A"="chats","WR-U-E-D"="sms","WR-U-E-E"="written assignments","WS-U-E-A"="auto cues","WS-U-T-B"="texts for the visually impaired"))

#change order of columns
#idioms <- idioms[,c(12,11,10,13,14,1,2,3,4,5,6,7,8,9)]

#remove CGN-annotations: IN NEW VERSION NOT IN THE CSV (EXCLUDED VIA OPENSONAR), SO THIS IS NOT NEEDED ANYMORE
#idioms <- idioms[!grepl("CGN document", idioms$doc_name),]

idioms$idiom_lemma <- tolower(idioms$idiom_lemma)

idioms$sentenceid <- paste(idioms$doc_id, word(idioms$xml_id,1,sep = ".w."),sep="-")

idioms <- idioms[idioms$doc_type_ != "auto cues",]

#FUNCTION OF JACOLIEN, MERG THIS ======================================

mostfreq <- function(x){
  tab <- sort(table(x))
  return(names(tail(tab,1)))
}

#mostfreq <- function(x){
#  tab <- sort(table(x))
#  return(paste(names(tab[tab==max(tab)]), collapse=";"))
#}

most_common <- ddply(idioms, "idiom_id", summarise,
                     most_common_lemma=paste(mostfreq(idiom_found),mostfreq(verb)))

#remove trailing space from most_common_lemma (added when adding optional verb)
most_common$most_common_lemma <- trimws(most_common$most_common_lemma)


idioms <- merge(idioms, most_common, by="idiom_id", all.x=TRUE)

idioms$idiom_length_orig <- word_count(idioms$most_common_lemma)
#with_verb = 1 if verb is included and 0 if no verb is included, this is added to the amount of words in the idiom_lemma
idioms$idiom_length_this <- word_count(idioms$idiom_lemma) + idioms$with_verb

#order idioms:
idioms <- idioms[order(idioms$id),]

id_most_common <- unique(data.frame(idioms$idiom_id,idioms$most_common_lemma))


#=====================================================================================================================================
#==========================================================ANALYZE IDIOM FREQS========================================================
#=====================================================================================================================================
library(plyr)
library(dplyr)

counts_per_collection <- ddply(idioms, .(doc_type_name), nrow)
counts_per_idiom <- ddply(idioms, .(idiom_id), nrow)
counts_per_idiom <- merge(counts_per_idiom,id_most_common,by.x="idiom_id",by.y="idioms.idiom_id")


#=====================================================================================================================================
#=============================================================ADD COLLECTION SIZES====================================================
#=====================================================================================================================================

texttype <- c("written assignments","policy documents","legal texts","books","subtitles","guides manuals","websites","reports","sms","chats","brochures","texts for the visually impaired","proceedings","press releases","discussion lists","teletext pages","e-magazines","newspapers","tweets","periodicals magazines","wikipedia","blogs","newsletters")
tokenfreq <- c(357947,8711551,10689681,26184781,28209846,236099,3111589,2218223,723876,11873434,1213382,675082,314025,332795,57070554,448865,8626248,211669748,23197211,93058924,23001184,139765,35446)
freqdf <- data.frame(texttype,tokenfreq)
tfreqdf <- t(freqdf)
colnames(tfreqdf) <- texttype

#=====================================================================================================================================
#=============================================================ADD AVG DOC LENGTHS ====================================================
#=====================================================================================================================================
avg_doc_length <- read.csv("results/avg_doc_length_new_media_correct.csv",header=TRUE,sep=";")

#=====================================================================================================================================
#==============================================================ADD IDIOM FEATURES=====================================================
#=====================================================================================================================================

fixedness_old_nv <- read.table("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\results\\fixedness\\fixedness_NV_COMPLETE_ID_DICT_JACOLIEN_LOWERED-set-of-idlists-after.txt", sep="\t")
fixedness_old_nn <- read.table("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\results\\fixedness\\fixedness_NN_COMPLETE_ID_DICT_JACOLIEN_LOWERED-set-of-idlists-after.txt", sep="\t")
fixedness_old <- rbind(fixedness_old_nn,fixedness_old_nv)


fixedness_nv <- read.table("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\results\\fixedness\\nounverbidioms-id-out-lowered-dict-set-of-idlists-after.txt", sep="\t")
fixedness_nn <- read.table("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\results\\fixedness\\twonounidioms-id-out-lowered-dict-set-of-idlists-after.txt", sep="\t")
fixedness <- rbind(fixedness_nn,fixedness_nv)

plot(fixedness_old$V4,fixedness$V4,xlab="Fixedness scores using existing lemma dictionary",ylab="Fixedness scores using custom lemma dictionary",main= "Fixedness scores reliability")

idiom_features <- merge(most_common, fixedness, by.x="idiom_id" ,by.y="V1")

idiom_features$V2 <- c()
idiom_features$V3 <- c()
colnames(idiom_features) <- c("id","idiom","fixedness")


#idiom_features <- merge(idiom_features,tprop_cross,by.x="idiom",by.y=0)

sprenger_feat <- read.csv("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\script files\\sprenger_data_own_ids.csv")
idiom_features <- merge(idiom_features,sprenger_feat,by.x="id",by.y="ID")
#idiom_features[, c(2:27)] <- sapply(idiom_features[, c(2:27)], as.numeric)

#=====================================================================================================================================
#==========================================================ANALYZE IDIOM FEATURES=====================================================
#=====================================================================================================================================

hist(idiom_features$fixedness)
qqnorm(idiom_features$fixedness)
qqline(idiom_features$fixedness)


hist(idiom_features$est.link)
qqnorm(idiom_features$est.link)
qqline(idiom_features$est.link)


#most_common_pos_head <- ddply(idioms, "idiom_id", summarise,
#                              most_common_pos_head=paste(mostfreq(pos_head)))

#idiom_features <- merge(idiom_features,most_common_pos_head,by.x="id",by.y="idiom_id")
#idiom_features$idiom_length <- sapply(strsplit(idiom_features$idiom, " "), length)

#=========================================================================================================================================
#===============================================================TEXT TYPE ANALYSIS========================================================
#=========================================================================================================================================

texttype <- c("discussion lists","e-magazines","e-newsletters","press releases","subtitles","teletext pages","websites","wikipedia","blogs","books","brochures","newsletters","guides manuals","legal texts","newspapers","periodicals magazines","policy documents","proceedings","reports","written assignments","texts for the visually impaired","tweets","chats","sms")
f = c(50.01945724800984, 66.65475989056685, 73.02779864763335, 73.1523267854659, 40.52451228840621, 58.311562135295404, 73.18003543837753, 70.45651852836734, 66.94659427043234, 52.230419066220136, 70.9003326020955, 63.39527680991095, 66.1826968194983, 77.15642454427714, 62.55577194192853, 63.60886727578005, 63.11894123144769, 66.61448814509454, 66.98679742196146, 64.34370590192398, 58.321571417219445,54.36,47.78,43.48)
ttr_lemmafreqlist <- c(0.013303222135847852,0.026542478259377656,0.3813249869587898,0.057401703751558765,0.009689241125244002,0.055542312276519665,0.024249025176525562,0.03766366983543108,0.11896397524415984,0.010572668146432082,0.04049095832969337,0.11157505443049301,0.03752663077776695,0.014107998171320548,0.010236219490373278,0.01398963091384981,0.010867410407170893,0.04554732903431256,0.024833391412856145,0.01861169390999226,0.046154393095949826,0.04369887397239263,0.018766516914988537,0.04383623714558847)
ttr_lemmaposfreqlist <- c(0.017683684714840903,0.03366492593303601,0.42618675013041213,0.07494102976306735,0.013140093001571153,0.07565526383210988,0.03161953587057931,0.04545992067190976,0.15152577540872178,0.0148120772902397,0.055242289732334915,0.14822989054251542,0.052668583941482175,0.017698750785921487,0.012530770339462963,0.018502535017490638,0.015308295847662488,0.06380383727410238,0.034785952539487686,0.02633071376488698,0.06540242518686619,0.05544851922069425,0.028243387717487627,0.05866198078123878)
ttr_wordfreqlist <- c(0.01503489926570883,0.03065284002963977,0.4298382889932186,0.07136525488664193,0.011863730131671048,0.07209071769908547,0.029865126788917173,0.03938271177692418,0.1471183772761421,0.01305071063989422,0.052791289140600405,0.1453666974857586,0.050038331377938915,0.016012825827075663,0.010854456159696471,0.01553254580936268,0.013587477132372868,0.060657590956134065,0.03182322065905908,0.024573470374105663,0.062094678868641145,0.04710152440308449,0.021423962098917636,0.053956755024341184)


lemmafreqlist_nopunct <- c(0.009737295449040926,0.02505633454871643,0.4146039603960396,0.05539288368037295,0.009999579483138052,0.056807600609849684,0.022206124793925162,0.03316707542401029,0.12524161834299807,0.010511150459190426,0.03863050056342701,0.11849272564297042,0.034720491735193953,0.011400996086308379,0.007381186650964399,0.011738154141236478,0.009008387454248272,0.04703320907552918,0.023697404645198903,0.01847072516573048,0.047058743893007696,0.024002900635746362,0.02078602786580707,0.050763790928889954)
wordfreqlist_nopunct <- c(0.011685934703758531,0.02987137984275055,0.47172156619018024,0.07085137820349519,0.01268776324726137,0.07566036526754245,0.028570021475272097,0.03526049767280581,0.15780551960252182,0.013336261889827403,0.052677158670495695,0.15712659145016045,0.04902756059421423,0.013660649405416059,0.008100505469133991,0.013452668449267709,0.011924140671838725,0.06403042541929216,0.03155241277456836,0.02565261992819156,0.06499910194150081,0.028123456839206782,0.023932062032854307,0.063149495375695)

cor.test(lemmafreqlist_nopunct,ttr_lemmafreqlist,method=c("pearson"))
#lemmafreqlist: p<0.001, r = 0.998, df = 22
cor.test(wordfreqlist_nopunct,ttr_wordfreqlist,method=c("pearson"))
#wordfreqlist: p<0.001, r = 0.999, df = 22


textfeats <- data.frame(texttype,f,ttr_lemmafreqlist,ttr_lemmaposfreqlist,ttr_wordfreqlist,lemmafreqlist_nopunct,wordfreqlist_nopunct)

plot(textfeats$ttr_lemmafreqlist,textfeats$lemmafreqlist_nopunct)
plot(textfeats$ttr_wordfreqlist,textfeats$wordfreqlist_nopunct)

#=====================================================================================================================================
#============================================= NEW: CALCULATE PROPFREQIDIOMS (with zeros) =========================================
#=====================================================================================================================================

cross_table <- as.data.frame.matrix(table(idioms$most_common_lemma,idioms$doc_type_name),)
cross_table$idiom = row.names(cross_table)
withzeros <- pivot_longer(cross_table[1:24], cols=1:23, names_to = "texttype", values_to = "freq")
withzeros <- merge(withzeros,most_common,by.x="idiom",by.y="most_common_lemma") #add id's to dataframe

withzeros = merge(withzeros,freqdf,by.x="texttype",by.y="texttype")
names(withzeros)[names(withzeros) == 'tokenfreq'] <- 'collsize'
withzeros$relfreq = (withzeros$freq/withzeros$collsize)*100000000

withzeros<-merge(withzeros,sprenger_feat[,c("ID","est")],by.x="idiom_id",by.y="ID")
withzeros<-merge(withzeros,textfeats[,c("texttype","f","ttr_wordfreqlist")],by.x="texttype",by.y="texttype")
withzeros<-merge(withzeros,idiom_features[,c("id","fixedness")],by.x="idiom_id",by.y="id")
withzeros<-merge(withzeros,avg_doc_length[,c("doc_type_name","meanlength")],by.x="texttype",by.y="doc_type_name")

names(withzeros)[names(withzeros) == 'ttr_wordfreqlist'] <- 'ttr'

withzeros$idiom_id <- as.factor(withzeros$idiom_id)
withzeros$idiom <- as.factor(withzeros$idiom)
withzeros$texttype <- as.factor(withzeros$texttype)
withzeros$present <- ifelse(withzeros$relfreq>0,1,0)
withzeros$relfreq <- (withzeros$freq/withzeros$collsize)*100000000
withzeros <- withzeros[withzeros$relfreq<50000,] # OUTLIERS REMOVED:
withzeros$logfreq <- log(withzeros$relfreq) # THIS TAKES AWAY A LOT OF VALUES BECAUSE LOG(0) DOES NOT EXIST


#=====================================================================================================================================
#============================================= NEW: CALCULATE PROPFREQIDIOMS (without zeros) =========================================
#=====================================================================================================================================

gooddata = ddply(idioms,.(idiom_id,doc_type_name),nrow)
gooddata = merge(id_most_common,gooddata,by.x="idioms.idiom_id",by.y="idiom_id")
colnames(gooddata)<-c("idiom_id","idiom","texttype","absfreq")

gooddata = merge(gooddata,freqdf,by.x="texttype",by.y="texttype")
names(gooddata)[names(gooddata) == 'tokenfreq'] <- 'collsize'
gooddata$relfreq = (gooddata$absfreq/gooddata$collsize)*100000000

gooddata<-merge(gooddata,sprenger_feat[,c("ID","est")],by.x="idiom_id",by.y="ID")
gooddata<-merge(gooddata,textfeats[,c("texttype","f","ttr_wordfreqlist")],by.x="texttype",by.y="texttype")
gooddata<-merge(gooddata,idiom_features[,c("id","fixedness")],by.x="idiom_id",by.y="id")
gooddata<-merge(gooddata,avg_doc_length[,c("doc_type_name","meanlength")],by.x="texttype",by.y="doc_type_name")

names(gooddata)[names(gooddata) == 'ttr_wordfreqlist'] <- 'ttr'

gooddata$idiom_id <- as.factor(gooddata$idiom_id)
gooddata$idiom <- as.factor(gooddata$idiom)
gooddata$texttype <- as.factor(gooddata$texttype)

qqnorm(gooddata$absfreq)
plot(gooddata$relfreq)
gooddata <- gooddata[gooddata$relfreq<50000,] # OUTLIERS REMOVED:
gooddata$logfreq <- log(gooddata$relfreq)
qqnorm(gooddata$logfreq)
qqline(gooddata$logfreq)

#gooddata$relfreq2 = (gooddata$absfreq/gooddata$collsize)
#gooddata$logfreq2 <- log(gooddata$relfreq2)
#tested whether the scaling influences the model / fvisgamplot, it does not

#m0 <- bam(logfreq ~ s(f)+ s(est) + ti(f,est), bs="re", data=gooddata) #this shows frequencies in a 
#more simplified way

#=========================================================================================================================================
# MAKE TEXTFEATS DF
#=========================================================================================================================================

textfeats <- merge(textfeats,counts_per_collection,by.x="texttype",by.y="doc_type_name",all.x=TRUE)
textfeats <- merge(textfeats,avg_doc_length,by.x="texttype",by.y="doc_type_name")
names(textfeats)[names(textfeats) == 'amountofwords'] <- 'collsize'
textfeats$totalrelfreq = (textfeats$V1/textfeats$collsize)*100000000
textfeats$totallogabsfreq <- log(textfeats$V1)
textfeats$totallogrelfreq = log(textfeats$totalrelfreq)
textfeats$tokenfreq <- textfeats$amountofword



#=========================================================================================================================================
# cobine gooddata with withzeros to analyze logrelfreq of ALL idioms instead of just the idioms that exist
#=========================================================================================================================================

alldata <- merge(withzeros,gooddata,all.x=TRUE)
alldata[alldata$freq==0,]$logfreq = log(((alldata[alldata$freq==0,]$freq+0.01)/alldata[alldata$freq==0,]$collsize)*100000000)
alldata$absfreq <- ifelse(is.na(alldata$absfreq),0,alldata$absfreq)

totalidiomfreqs <- merge(counts_per_idiom,id_most_common,by.x="idiom_id",by.y= "idioms.idiom_id")
#=========================================================================================================================================
# PCA ATTEMPT
#=========================================================================================================================================
#alldata.pca <- prcomp(alldata[,c(3:15,18:27,29,31)], center = TRUE,scale. = TRUE)
#alldata.pca2 <- princomp(alldata[,c(3:15,18:27,29,31)], cor = TRUE,scores = TRUE)

#summary(alldata.pca)
#fviz_eig(alldata.pca,label=TRUE)

#=====================================================================================================================================
#=========================================================== CLUSTER TEXT TYPES ======================================================
#=====================================================================================================================================

#TODO: ADD ALL FEATURES AGAIN
fviz_nbclust(textfeats[,c(2,5,7,9)], kmeans, method = "gap_stat")

set.seed(123)
km.res <- kmeans(textfeats[,c(2,5,7,9)], 5, nstart = 100)
# Visualize

fviz_cluster(km.res, data = textfeats[,c(2,5,7,9)],
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

# Compute hierarchical clustering
res.hc <- textfeats[,c(2,5,7,9)] %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering

res.hc$labels <- textfeats$texttype

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 5, # Cut in 8 groups (optimal # of clusters)
          cex = 0.9, # label size
          #k_colors = rainbow(11),
          color_labels_by_k = TRUE # color labels by groups
          #rect = TRUE, # Add rectangle around groups
)



#=====================================================================================================================================
#====================================================== GET MEAN CLUSTER FEATURE VALUES ==============================================
#=====================================================================================================================================

# Cut tree into 4 groups
sub_grp <- cutree(res.hc, k = 5)

table(sub_grp)


clusterinfo <- textfeats %>% mutate(cluster = sub_grp)
meanclusterdata <- ddply(clusterinfo, .(cluster), numcolwise(mean))

sub_grp <- data.frame(sub_grp)
sub_grp$texttype <- rownames(sub_grp)

meanclusterdata <- merge(meanclusterdata,sub_grp,by.x="cluster",by.y="sub_grp")

meanclusterdata <- aggregate(texttype ~ ., meanclusterdata, toString)

meanclusteroutput <- meanclusterdata[,c(1,2,5,7,9,15)]

meanclusteroutput

#=========================================================================================================================================
# LOOK INTO STRUCTURE OF THE DATA
#=========================================================================================================================================

plot(alldata$f,alldata$relfreq,xlab="Formality score",ylab="Idiom frequency per 100 million words",main="Correlation of idiom frequency with formality score")
plot(alldata$ttr,log(alldata$relfreq),xlab="Type-token Ratio",ylab="Idiom Frequency per 100 million words",main="Idiom Frequency X Type-token Ratio (TTR)")
plot(alldata$est,log(alldata$relfreq),xlab="Estimated Decomposability",ylab="Idiom Frequency per 100 million words",main="Idiom Frequency X Decomposability")
plot(alldata$fixedness,log(alldata$relfreq),xlab="Lexical Fixedness",ylab="Idiom Frequency per 100 million words",main="Idiom Frequency X Lexical Fixedness")

plot(log(alldata$collsize),log(alldata$relfreq),xlab="Normalized collection size",ylab="Idiom Frequency per 100 million words",main="Idiom Frequency X Normalized collection size")
#THIS EFFECT IS STRAIGHTFORWARD: the collection size is used to calculate the relative idiom frequency, therefore there is a straight line



#=========================================================================================================================================
# RELFREQ INCLUDING ZEROS
#=========================================================================================================================================

mbase <- bam(logfreq ~ s(f)+ s(idiom, bs="re")+s(texttype, bs="re"), data=alldata)
mbase2 <- bam(logfreq ~ s(fixedness)+ s(idiom, bs="re")+s(texttype, bs="re"), data=alldata)
mbase3 <- bam(logfreq ~ s(ttr)+ s(idiom, bs="re")+s(texttype, bs="re"), data=alldata)
AIC(mbase,mbase2,mbase3)
summary(mbase)
summary(m0)


#FIRST COMPARE ALL FIXED EFFECTS SEPARATELY
m0_1 <- bam(logfreq ~ s(f)+ s(idiom, bs="re")+s(texttype, bs="re"), method="ML",data=alldata)
m0_1_2 <- bam(logfreq ~ s(f)+ s(idiom, bs="re"), method="ML",data=alldata)
plot_smooth(m0_1_2,view="f")
summary(m0_1_2)
qqnorm(resid(m0_1_2))
qqline(resid(m0_1_2))

qqnorm(resid(m0_1))
qqline(resid(m0_1))



m0_2 <- bam(logfreq ~ s(est)+ s(idiom, bs="re")+s(texttype, bs="re"), method="ML", data=alldata) #BEST MODEL
m0_3 <- bam(logfreq ~ s(ttr)+ s(idiom, bs="re")+s(texttype, bs="re"), method="ML", data=alldata)
m0_4 <- bam(logfreq ~ s(fixedness)+ s(idiom, bs="re")+s(texttype, bs="re"), method="ML",data=alldata)
m0_5 <- bam(logfreq ~ s(collsize)+ s(idiom, bs="re")+s(texttype, bs="re"), method="ML",data=alldata)
m0_6 <- bam(logfreq ~ s(meanlength)+ s(idiom, bs="re")+s(texttype, bs="re"), method="ML",data=alldata)

summary(m0_1)

m0_7 <- bam(logfreq ~ s(f)+ s(idiom, bs="re")+s(texttype,idiom, bs="re"), method="ML",data=alldata)
compareML(m0_1,m0_2) #m01 wins
compareML(m0_1,m0_3) #small difference, m01 lower AIC
compareML(m0_1,m0_4) #m01 wins
compareML(m0_1,m0_5) #small difference, m05 lower AIC
compareML(m0_1,m0_6) #m01 wins

hist(resid(m0_1))
qqnorm(resid(m0_1))
qqline(resid(m0_1))
plot(resid(m0_1))


#Formality clearly gives the best models, followed by collsize and ttr (All text features)
#Adding more variables to that model:
m0_7 <- bam(logfreq ~ s(f) + s(ttr) + s(idiom, bs="re")+s(texttype, bs="re"), method="ML",data=alldata)

compareML(m0_1,m0_7) #small difference, m01 lower AIC

summary(m0_7)
m0_7_2 <- bam(logfreq ~ s(ttr) + s(idiom, bs="re")+s(texttype, bs="re"), method="ML",data=alldata)
summary(m0_7_2)

hist(resid(m0_7))
qqnorm(resid(m0_7))
qqline(resid(m0_7))
plot(resid(m0_7))


m0_8 <- bam(logfreq ~ s(f) + s(collsize) + s(idiom, bs="re")+s(texttype, bs="re"), method="ML",data=alldata)
compareML(m0_1,m0_8) #small difference, m01 lower AIC



#TRY MORE SMOOTH PLOTS LIKE THIS BETWEEN SIMILAR MODELS
plot_smooth(m0_1, view="f", col="blue", rm.ranef=TRUE)
plot_smooth(m0_8, view="f", col="red", rm.ranef=TRUE,add=TRUE)
#adding all kinds of extra variables does not make a significant change to the fit: the confidence bands overlap

#Only small differences OR m0_! is best, Formality score shows difference in best way
#ML shows the difference better  than AIC because of different fixed effects, 
#other text type features give models that are similar to Formality score model but the ttr is not significant and collsize is 
#less significant thatn Formality score

plot_smooth(m0,view="f",rm.ranef = TRUE)
summary(m0)


m0_9 <- bam(logfreq ~ s(f) + s(collsize) + ti(f,collsize) + s(idiom, bs="re")+s(texttype, bs="re"), method="ML",data=alldata)
compareML(m0_1,m0_9)#small difference, m01 lower AIC

m0_10 <- bam(logfreq ~ s(f) + s(est) + s(idiom, bs="re")+s(texttype, bs="re"), method="ML",data=alldata)
m0_10_2 <- bam(logfreq ~ s(f) + s(est) + s(idiom, bs="re")+s(texttype, bs="re"), method="ML",data=alldata[alldata$texttype!="texts for the visually impaired",])
m0_10_3 <- bam(logfreq ~ s(f) + s(est) + s(idiom, bs="re")+s(texttype, bs="re"), method="ML",data=alldata[alldata$texttype!="sms"&alldata$texttype!="chats"&alldata$texttype!="tweets",])
AIC(m0_10,m0_10_2)


testdata = data=alldata[alldata$texttype!="sms"&alldata$texttype!="guides manuals"&alldata$texttype!="legal texts"&alldata$texttype!="written assignments",]
m0_10_4_try_this <- bam(logfreq ~ s(meanlength) + s(est) + s(idiom, bs="re")+s(texttype, bs="re"), method="ML",data=testdata)
summary(m0_10_4_try_this)
plot_smooth(m0_10_4_try_this,view ="f")


plot_smooth(m0_10,view ="f")
plot_smooth(m0_10,view ="f",xlab="Formality score",ylab="Normalized idiom frequency",main="The fitted effects of the model")
plot_smooth(m0_10_3,view ="f",xlab="Formality score",ylab="Normalized idiom frequency")
fvisgam(m0_10,view=c("f","est"))
fvisgam(m0_10_3,view=c("f","est"))

gam.check(m0_10)
hist(resid(m0_10))
plot(resid(m0_10))


compareML(m0_1,m0_10)#small difference, m01 lower AIC

m0_11 <- bam(logfreq ~ s(f) + s(est) + ti(f,est) + s(idiom, bs="re")+s(texttype, bs="re"), method="ML",data=alldata)
compareML(m0_1,m0_11)#small difference, M11 LOWER AIC, but est not significant and ti(f,est) only significant on 0.1 level

oldmar <- par()$mar
par(mar=oldmar + c(0,0,0,1) ) # add one line to the right
fvisgam(m0_10,view=c("f","est"), xlab = "Formality score", ylab = "Estimated decomposability", main = "Normalized idiom frequency")

plot_smooth(m0_10,view="f")

#=======================================================================================================================
#================================================models per type=======================================================
#=======================================================================================================================

plot(fitted(m0_1),fitted(m0_11))
plot(fitted(m0_1),resid(m0_1))
qqnorm(resid(m0_1))
qqline(resid(m0_1))

plot(fitted(m0_2),resid(m0_2))


gam.check(m0)
plot(resid(m0))

qqnorm(resid(m0))
qqline(resid(m0))

#=====================================================================================================================================
#============================================MAKE NEW STATISTICAL MODELS - binomial===================================================
#=====================================================================================================================================
# 
# m0_logistic <- bam(present ~ s(f) + s(idiom, bs="re")+s(texttype, bs="re"), data=withzeros, family="binomial", method="ML")
# 
# 
# summary(m0_logistic)
# m1_logistic <- bam(present ~ s(ttr) + s(idiom, bs="re")+s(texttype, bs="re"), data=withzeros, family="binomial",method="ML")
# 
# compareML(m0_logistic,m1_logistic) 
# summary(m1_logistic) 
# #m0_logistic better, but TTR is significant as well when included
# 
# m2_logistic <- bam(present ~ s(collsize) + s(idiom, bs="re")+s(texttype, bs="re"), data=withzeros, family="binomial",method="ML")
# compareML(m0_logistic,m2_logistic)
# #m2_logistic better: bigger collection -> chance that an idiom is present SOMEWHERE increases
# 
# m3_logistic <- bam(present ~ s(est) + s(idiom, bs="re")+s(texttype, bs="re"),data=withzeros, family="binomial",method="ML")
# compareML(m2_logistic,m3_logistic)
# 
# m4_logistic <- bam(present ~ s(fixedness) + s(idiom, bs="re")+s(texttype, bs="re"), data=withzeros, family="binomial",method="ML")
# compareML(m2_logistic,m4_logistic)
# 
# m5_logistic <- bam(present ~ s(ttr) + s(idiom, bs="re")+s(texttype, bs="re"), data=withzeros, family="binomial",method="ML")
# compareML(m2_logistic,m5_logistic)
# 
# m6_logistic <- bam(present ~ s(meanlength) + s(idiom, bs="re")+s(texttype, bs="re"), data=withzeros, family="binomial",method="ML")
# compareML(m2_logistic,m6_logistic)
# 
# #in logistic models, the collsize is more important and (marginal) significance of ttr is explained by correlation with collsize


#=========================================================================================================================================
#STATISTICAL ANALYSIS BASED ON CLUSTER VALUES
#=========================================================================================================================================
typeclust <- separate_rows(meanclusterdata, texttype, sep = ", ")
typeclust <- typeclust[,c(1,15)]
gooddata_clust <- merge(gooddata,typeclust)
gooddata_clust$cluster <- as.factor(gooddata_clust$cluster)

model_cluster <- bam(logfreq ~ cluster,data=gooddata_clust)
#summary(model_cluster)

model_textype <- bam(logfreq ~ texttype,data=gooddata_clust)
#summary(model_textype)

mean(gooddata_clust[gooddata_clust$cluster == "1",]$logfreq)
mean(gooddata_clust[gooddata_clust$cluster == "2",]$logfreq)
mean(gooddata_clust[gooddata_clust$cluster == "3",]$logfreq)
mean(gooddata_clust[gooddata_clust$cluster == "4",]$logfreq)
mean(gooddata_clust[gooddata_clust$cluster == "5",]$logfreq)

#in cluster 1 way more idioms than in other groups
#cluster 1: blogs, newsletters: not very formal, not very short

#=========================================================================================================================================
#PLOTS FOR PAPER/PRESENTATION
#=========================================================================================================================================
i=0

ggscatter(textfeats, x = "V1", y = "f", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between Idiom Freq. and Formality Score \n(Pearson)", xlab = "Idiom Frequency", ylab = "Formality Score")

i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))

ggscatter(textfeats, x = "V1", y = "tokenfreq", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between Idiom Freq. and Collection Size \n(Pearson)", xlab = "Idiom Frequency", ylab = "Collection Size")


i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))

ggscatter(textfeats, x = "V1", y = "ttr_wordfreqlist", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between Idiom Freq. and Type-token Ratio (word level) \n(Pearson)", xlab = "Idiom Frequency", ylab = "Type-token Ratio")


i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))

ggscatter(textfeats, x = "f", y = "ttr_wordfreqlist", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between F-score and Type-token Ratio (word level) \n(Pearson)", xlab = "F-score", ylab = "Type-token Ratio")

i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))


ggscatter(textfeats, x = "totallogabsfreq", y = "f", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between log Idiom Freq. and Formality Score \n(Pearson)", xlab = "Log Idiom Frequency", ylab = "Formality Score")

i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))

ggscatter(textfeats, x = "totallogabsfreq", y = "tokenfreq", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between log Idiom Freq. and Collection Size \n(Pearson)", xlab = "Log Idiom Frequency", ylab = "Collection Size")

i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))

textfeats$logcollsize <- log(textfeats$collsize)

ggscatter(textfeats, x = "totallogabsfreq", y = "logcollsize", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between log Idiom Freq. and log Collection Size \n(Pearson)", xlab = "Log Idiom Frequency", ylab = "Log Collection Size")

i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))

ggscatter(textfeats, x = "totallogabsfreq", y = "ttr_wordfreqlist", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between log Idiom Freq. and Type-token Ratio (word level) \n(Pearson)", xlab = "Idiom Frequency", ylab = "Type-token Ratio")

i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))

plot(textfeats$f, textfeats$ttr_wordfreqlist, xlab="Formality Score", ylab="Type-Token Ratio", main="Type-token Ratio X Formality score")



i=i+1
ggsave(paste("results/correlations/",i,".png",sep=""),width=30,height=15,units=c("cm"))


ggscatter(textfeats, x = "V1", y = "collsize", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between Idiom Freq. and Collection Size\n(Pearson)", xlab = "Idiom Frequency", ylab = "Collection Size")




meanlengthf <- unique(data.frame(gooddata$f,gooddata$meanlength))
colnames(meanlengthf) <- c("f","meanlength")
plot(meanlengthf$f,meanlengthf$meanlength)


plot(meanlengthf$f,meanlengthf$meanlength,xlab="Formality score",ylab="Mean document length")
ggscatter(meanlengthf, x = "f", y = "meanlength", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between formality score and mean document length \n(Pearson)", xlab = "Formality score", ylab = "Mean document length")


meanlengthf$logmeanlength <- log(meanlengthf$meanlength)
plot(meanlengthf$f,meanlengthf$logmeanlength,xlab="Formality Score",ylab="Normalized Mean Text Length")
ggscatter(meanlengthf, x = "f", y = "logmeanlength", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title= "Correlation between Formality Score and Normalized Mean Text Length \n(Pearson)", xlab = "Formality Score", ylab = "Average text length")



#======================================================

ggplot(data=textfeats, aes(x=reorder(texttype,f),y=f,fill=f)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + xlab("Text type") +
  geom_bar(position="dodge",stat="identity") + 
  ggtitle("Formality scores for all text types") +
  labs(x="Text type",y="Formality score", fill="Formality\nscore") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17)) + My_Theme

ggplot(data=gooddata, aes(x=reorder(texttype,f),y=ttr, fill=f)) + 
  geom_bar(position="dodge",stat="identity") + 
  ggtitle("Type-token ratio for all text types") +
  labs(x="Text type",y="Type-token ratio") + My_Theme +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17) )

ggplot(data=textfeats, aes(x=reorder(texttype,f),y=ttr_wordfreqlist,fill=f)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + xlab("Text type") +
  geom_bar(position="dodge",stat="identity") + 
  ggtitle("Type-token ratios for all text types") +
  labs(x="Text type",y="TTR", fill="F-score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 17)) + My_Theme

ggplot(textfeats,
       aes(y=log(meanlength), x=reorder(texttype, f),fill=f)) + geom_bar(stat="identity", width=0.7) + xlab("Text type") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17)) + ggtitle("Normalized average text size of each collection") + My_Theme

ggplot(textfeats,
       aes(y=log(collsize), x=reorder(texttype, f),fill=f)) + geom_bar(stat="identity", width=0.7) + xlab("Text type") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17)) + ggtitle("Normalized collection size") + My_Theme


ggplot(idiom_features[idiom_features$fixedness<1.60,],
       aes(y=fixedness, x=reorder(idiom,fixedness))) + geom_bar(stat="identity", width=0.7) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17)) + ggtitle("Lexical Fixedness") + 
  coord_cartesian(ylim=c(-1.1,3.1)) + My_Theme

ggplot(idiom_features[idiom_features$fixedness>1.60,],
       aes(y=fixedness, x=reorder(idiom,fixedness))) + geom_bar(stat="identity", width=0.7) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17)) + ggtitle("Lexical Fixedness") + 
  coord_cartesian(ylim=c(-1.1,3.1)) 

#==================================

plot(gooddata$ttr,gooddata$relfreq)

ggplot(gooddata,
       aes(y=absfreq, x=reorder(texttype, f))) + geom_bar(stat="identity", width=0.7) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17)) + ggtitle("Absolute total frequency of idioms per collection") + My_Theme



ggplot(gooddata, 
       aes(y=absfreq, x=reorder(texttype, f), fill = f)) + geom_bar(position="stack", stat="identity", width=0.7) + labs(x="Text type",y="Absolute frequency",fill="Formality score") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17) ) + ggtitle("Absolute total frequency of idioms per collection") + My_Theme

ggplot(gooddata, 
       aes(y=relfreq, x=reorder(texttype, f), fill=ttr)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + labs(x="Text Type",y="Relative idiom frequency", fill ="TTR") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17) ) + ggtitle("Relative total frequency of idioms per collection") + My_Theme

ggplot(gooddata, 
       aes(y=relfreq, x=reorder(texttype, f), fill=ttr)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + labs(x="Text Type",y="Relative freq.", fill ="TTR") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17) ) + ggtitle("Relative total frequency of idioms per collection") + My_Theme

ggplot(gooddata, 
       aes(y=relfreq, x=reorder(texttype, ttr), fill=ttr)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + labs(x="Text Type",y="Relative freq.", fill ="TTR") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17) ) + ggtitle("Relative total frequency of idioms per collection") + My_Theme

#plot for idiomfreq results

library(stringr)

gooddata$texttype <- str_to_sentence(gooddata$texttype)
ggplot(gooddata, 
       aes(y=relfreq, x=reorder(texttype,texttype))) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + labs(x="Text type",y="Relative idiom frequency", fill="Formality score") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17) ) + ggtitle("Relative total frequency of idioms per collection") + My_Theme

#plot for modelling results
ggplot(gooddata, 
       aes(y=relfreq, x=reorder(texttype, f), fill=f)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + labs(x="Text type",y="Relative frequency", fill="Formality score") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17) ) + ggtitle("Relative total frequency of idioms per collection") + My_Theme

ggplot(gooddata, 
       aes(y=relfreq, x=reorder(texttype, relfreq,FUN = sum))) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + labs(x="Text Type",y="Relative freq.", fill ="F-score") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17) ) + ggtitle("Relative total frequency of idioms per collection") + My_Theme

ggplot(gooddata, 
       aes(y=relfreq, x=reorder(texttype, f))) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + xlab("Text type") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17) ) + ggtitle("Relative total frequency of idioms per collection") + labs(fill="F-score") + My_Theme

ggplot(gooddata, 
       aes(y=relfreq, x=reorder(texttype, f), fill=f)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + xlab("Text type") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17) ) + ggtitle("Relative total frequency of idioms per collection") + labs(fill="F-score") + My_Theme

ggplot(gooddata, 
       aes(y=relfreq, x=reorder(texttype, f), fill=ttr)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + xlab("Text type") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17) ) + ggtitle("Relative total frequency of idioms per collection") + labs(fill="ttr") + My_Theme

ggplot(gooddata, 
       aes(y=log(relfreq), x=reorder(texttype, f), fill=f)) + scale_color_gradient() + geom_bar(position="stack", stat="identity", width=0.7) + xlab("Text type") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust=0.2, size = 17) ) + ggtitle("Normalized relative total frequency of idioms per collection") + labs(fill="F-score") + My_Theme





#=====================================================================================================================================
#===========================================OLD: MAKE NEW STATISTICAL MODELS - zeros excluded ========================================
#=====================================================================================================================================
# #gooddata$logfreq10 <- log10(gooddata$relfreq)
# 
# m0_without_zeros <- bam(logfreq ~ s(f)+ s(est) + ti(f,est) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
# hist(resid(m0_without_zeros))
# 
# 
# plot(m0_without_zeros,scale=0)
# summary(m0_without_zeros)
# library(mgcv)
# 
# oldmar <- par()$mar
# par(mar=oldmar + c(0,0,0,2) ) # add one line to the right
# fvisgam(m0_without_zeros,view=c("f","est"),dec=1,xlab="Formality Score",ylab="Estimated decomposability")
# 
# 
# par(mar=oldmar) # restore to default settings
# 
# plot_smooth(m0_without_zeros,view="f",cond=list(est=4))
# plot_smooth(m0_without_zeros,view="f",cond=list(est=2.5),add=TRUE,col=2)
# 
# pvisgam(m0_without_zeros,select=4,view=c("f","est"),dec=1,too.far = 0.03)
# plot(m0_without_zeros,scale=0,select=4)
# summary(m0_without_zeros)
# gam.check(m0_without_zeros)
# 
# #m1_norand <- bam(logfreq ~ s(est), data=gooddata)
# #m2_norand <- bam(logfreq ~ s(f), data=gooddata)
# #m3_norand <- bam(logfreq ~ s(f)+s(est), data=gooddata)
# #m4_norand <- bam(logfreq ~ s(f,est),data=gooddata)
# #m5_norand <- bam(logfreq ~ s(meanlength),data=gooddata)
# #summary(m3_norand)
# #AIC(m1_norand,m2_norand,m3_norand,m4_norand,m5_norand)
# 
# #plot_smooth(m1_norand,view="est")
# #plot_smooth(m2_norand,view="f")
# #plot_smooth(m3_norand,view=c("f","est"),add=TRUE,col=2)
# #plot_smooth(m4_norand,view=c("f","est"))
# #plot_smooth(m5_norand,view=c("meanlength"),add=TRUE,col=3)
# 
# str(gooddata)
# m1_without_zeros <- bam(logfreq ~ s(f) + s(est) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
# 
# m2_without_zeros <- bam(logfreq ~ s(f) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
# m3_without_zeros <- bam(logfreq ~ s(est) + s(fixedness) + s(idiom, bs="re")+s(texttype, bs="re"),data=gooddata)
# m4_without_zeros <- bam(logfreq ~ s(fixedness) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
# m5_without_zeros <- bam(logfreq ~ s(est) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
# m6_without_zeros <- bam(logfreq ~ s(est) + s(ttr) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
# m7_without_zeros <- bam(logfreq ~ s(f) + s(ttr) + s(idiom, bs="re") +s(texttype, bs="re"), data=gooddata) #+ s(idiom, bs="re")
# m8_without_zeros <- bam(logfreq ~ s(f) + s(est) + s(meanlength) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
# #m9_without_zeros <- bam(logfreq ~ s(f) + s(est) + + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata[gooddata$doc_type_name!="tweets"&&gooddata$doc_type_name!="chats"&&gooddata$doc_type_name!="sms",])
# m10_without_zeros <- bam(logfreq ~ s(ttr) + s(est) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
# m11_without_zeros <- bam(logfreq ~ s(f) + s(est) + s(meanlength) + s(ttr) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
# 
# gooddata$logcollsize_without_zeros <- log(gooddata$collsize)
# m12_without_zeros <- bam(logfreq ~ s(logcollsize_without_zeros) + s(est) + s(idiom, bs="re")+s(texttype, bs="re"), data=gooddata)
# 
# 
# oldmar <- par()$mar
# par(mar=oldmar + c(0,0,0,2) ) # add one line to the right
# fvisgam(m0_without_zeros,view=c("f","est"),dec=1,xlab="Formality Score",ylab="Estimated decomposability")
# par(mar=oldmar) # restore to default settings
# 
# oldmar <- par()$mar
# par(mar=oldmar + c(0,0,0,2) ) # add one line to the right
# fvisgam(m6_without_zeros,view=c("ttr","est"),dec=1,xlab="Type-Token Ratio",ylab="Estimated decomposability")
# par(mar=oldmar) # restore to default settings
# 
# 
# oldmar <- par()$mar
# par(mar=oldmar + c(0,0,0,2) ) # add one line to the right
# fvisgam(m8_without_zeros,view=c("meanlength","est"),dec=1,xlab="Type-Token Ratio",ylab="Estimated decomposability")
# par(mar=oldmar) # restore to default settings
# 
# oldmar <- par()$mar
# par(mar=oldmar + c(0,0,0,3) ) # add one line to the right
# fvisgam(m12_without_zeros,view=c("logcollsize_without_zeros","est"),dec=1,xlab="Collection Size",ylab="Estimated decomposability")
# par(mar=oldmar) # restore to default settings
# 
# AIC(m0_without_zeros,m1_without_zeros,m2_without_zeros,m3_without_zeros,m4_without_zeros,m5_without_zeros,
#     m6_without_zeros,m7_without_zeros,m8_without_zeros,m10_without_zeros,m11_without_zeros,m12_without_zeros)
# 
# 
# pvisgam(m0_without_zeros,select=4,view=c("f","est"),dec=1,too.far = 0.3)
# summary(m0_without_zeros,) #ttr is more informative than f in this case
# 
# plot_smooth(m0_without_zeros, view="f")
# plot_smooth(m0_without_zeros, view="est")
# pvisgam(m0_without_zeros,select=4,view=c("f","est"),dec=1,too.far = 0.1)
# fvisgam(m0_without_zeros,view=c("f","est"),dec=1)
# 
# gam.check(m0_without_zeros,)
# plot(resid(m0_without_zeros,))
# 
# mean(gooddata[gooddata$f>65 &gooddata$f<70&gooddata$est>2&gooddata$est <2.5,]$logfreq) #test plot output with real data
# 
# #plot(gooddata$logfreq,predict(m7)) #this is part of gam.check in a better way
# 
# plot(gooddata$relfreq,gooddata$f)

#----------------------------#
##
#----------------------------#

#=====================================================================================================================================
#=====================================================Counts for discussion section ==================================================
#=====================================================================================================================================
sum(alldata[alldata$texttype=="sms",]$freq)
nrow(alldata[alldata$texttype=="sms"&alldata$relfreq==1,])
#=====================================================================================================================================
#=====================================================================================================================================
#=====================================================================================================================================

