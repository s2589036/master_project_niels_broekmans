pos=read.csv("G:\\Mijn Drive\\Studie informatiekunde\\master\\master project\\project\\script files\\collection_pos_freq.csv",sep=";",stringsAsFactors = TRUE)
posprop <- as.data.frame(prop.table(as.matrix.data.frame(pos[-1]),1))
posprop$texttype <- tolower(pos$X)

texttype <- c("discussion lists","e-magazines","e-newsletters","press releases","subtitles","teletext pages","websites","wikipedia","blogs","books","brochures","newsletters","guides manuals","legal texts","newspapers","periodicals magazines","policy documents","proceedings","reports","written assignments","texts for the visually impaired","tweets","chats","sms")
f = c(50.01945724800984, 66.65475989056685, 73.02779864763335, 73.1523267854659, 40.52451228840621, 58.311562135295404, 73.18003543837753, 70.45651852836734, 66.94659427043234, 52.230419066220136, 70.9003326020955, 63.39527680991095, 66.1826968194983, 77.15642454427714, 62.55577194192853, 63.60886727578005, 63.11894123144769, 66.61448814509454, 66.98679742196146, 64.34370590192398, 58.321571417219445,54.36,47.78,43.48)
fscores <- data.frame(texttype,f)

posprop$possum1 <- pos$possum1
posprop$formality_orig <- pos$formality_orig
posprop$collection <- pos$X


posprop <- merge(posprop,fscores,by.x="texttype",by.y="texttype")





library(ggplot2)
plot.new()
ggplot(posprop,aes(x=f,y=ADJ*100)) + geom_point() + labs(title="Adjectives", x="Formality score per Heylighen and Deweale", y = "% of adjectives in text type") + ylim(0, 25) + geom_smooth(method='lm')
ggplot(posprop,aes(x=f,y=BW*100)) + geom_point() + labs(title="Adverbs", x="Formality score per Heylighen and Deweale", y = "% of adverbs in text type") + ylim(0, 25) + geom_smooth(method='lm')
ggplot(posprop,aes(x=f,y=LID*100)) + geom_point() + labs(title="Determiners", x="Formality score per Heylighen and Deweale", y = "% of determiners in text type") + ylim(0, 25) + geom_smooth(method='lm')
ggplot(posprop,aes(x=f,y=ZNW*100)) + geom_point() + labs(title="Nouns", x="Formality score per Heylighen and Deweale", y = "% of nouns in text type") + ylim(0, 25) + geom_smooth(method='lm')
ggplot(posprop,aes(x=f,y=TSW*100)) + geom_point() + labs(title="Interjections", x="Formality score per Heylighen and Deweale", y = "% of interjections in text type") + ylim(0, 25) + geom_smooth(method='lm')
ggplot(posprop,aes(x=f,y=VNW*100)) + geom_point() + labs(title="Pronouns", x="Formality score per Heylighen and Deweale", y = "% of pronouns in text type") + ylim(0, 25) + geom_smooth(method='lm')
ggplot(posprop,aes(x=f,y=VZ*100)) + geom_point() + labs(title="Prepositions", x="Formality score per Heylighen and Deweale", y = "% of prepositions in text type") + ylim(0, 25) + geom_smooth(method='lm')
ggplot(posprop,aes(x=f,y=WW*100)) + geom_point() + labs(title="Verbs", x="Formality score per Heylighen and Deweale", y = "% of verbs in text type") + ylim(0, 25) + geom_smooth(method='lm')
ggplot(posprop,aes(x=f,y=LET*100)) + geom_point() + labs(title="Punctuation", x="Formality score per Heylighen and Deweale", y = "% of interpunction in text type") + ylim(0, 25) + geom_smooth(method='lm')
ggplot(posprop,aes(x=f,y=SPEC*100)) + geom_point() + labs(title="Special", x="Formality score per Heylighen and Deweale", y = "% of spec in text type") + ylim(0, 25) + geom_smooth(method='lm')
ggplot(posprop,aes(x=f,y=TW*100)) + geom_point() + labs(title="Numerals", x="Formality score per Heylighen and Deweale", y = "% of numerals in text type") + ylim(0, 25) + geom_smooth(method='lm')
ggplot(posprop,aes(x=f,y=VG*100)) + geom_point() + labs(title="Conjunctions", x="Formality score per Heylighen and Deweale", y = "% of conjunctions in text type") + ylim(0, 25) + geom_smooth(method='lm')


# 2"ADJ"              3"BW"               4"LID"              5"ZNW"              6"TSW"              7"VNW"             
# 8"VZ"               9"WW"               10"LET"              11"SPEC"             12"TW"               13"VG"


#sum the relevant pos-tag frequencies
pos$possum_chosen = rowSums(pos[, c(2,3,4,5,6,7,8,9)])

pos$possum_all = rowSums(pos[, c(2,3,4,5,6,7,8,9,10,11,12,13)])

#calculate formality score using some formal categories and some informal categories 
pos$formality_chosen = (
  #formal categories
  (rowSums(pos[, c(2,4,5,8)])/pos$possum_chosen)*100 
  
  #informal categories
  - (rowSums(pos[, c(3,6,7,9)])/pos$possum_chosen)*100 
  +100)/2

#FULL ENGLISH NAMES
#colnames(pos)[1:13] <- c("collection","adjective","adverb","determiner","noun","interjection","pronoun","preposition","verb","punctuation","names/unknown","numerator","conjunction")
colnames(pos)[1:13] <- c("collection","adj","adv","det","noun","interj","pron","prep","verb","punct","names","num","conj")

#==================================================================MAKE BARPlOTS============================================================

#Formal: 5 noun, 2 adj, 8 prep, 4 article
#Informal: 7 pronoun, 9 verb, 3 adverb, 6 interjection
#Nothing: 10 let, 11 spec, 12 tw, 13 vg

barplotdata <- pos[,1:13]
barplotprop <- prop.table(as.matrix(barplotdata[,2:13]), margin = 1) * 100
barplotprop <- barplotprop[,c(4,1,7,3,6,8,2,5,9,10,11,12)]


for(i in 1:23){
  mypath <- paste("G:/Mijn Drive/Studie informatiekunde/master/master project/paper/images/formplots_english_bw/",barplotdata$collection[i],".png",sep="")
  png(file=mypath)
  
  #original colours: red, blue, grey
   barplot(barplotprop[i,],ylim=range(pretty(c(0, 30))),col=c(rep("black",4), rep("white",4), rep("grey",4)), 
          main=barplotdata$collection[i],xlab = "Part-of-speech tag", ylab="% of words in collection", las=2)
  legend("topright", legend=c("Formal", "Informal","Unknown"),fill=c("black", "white","grey")) 
  
  dev.off()
}

#colnames <- c("adj","adv","det","noun","interj","pron","prep","verb","names","num","conj") #punctuation not taken into account
#total_words <- rowSums(pos[,colnames]) #all pos-tags summed (empty pos tags not taken into account because those were not words)
#total_files <- c(708600,702091,7860,778,507,86,18699,12,9,176043,216,1053,18,81,8368,93,944,956,124124,188) #info from sonar documentation (folia files)
#coll_info <- data.frame(pos$collection,total_words,total_files)
#coll_info$avg_text_length <- round(coll_info$total_words/coll_info$total_files)


#--------------------------------------------CALCULATE CUSTOM FORMALITY SCORES -----------------------------------------------------#

#Formal: 5 noun, 2 adj, 8 prep, 4 article
#Informal: 7 pronoun, 9 verb, 3 adverb, 6 interjection
#Nothing: 10 let, 11 spec, 12 tw, 13 vg

formalitydf <- data.frame(pos$collection)

pos$possum1 = pos$ZNW + pos$ADJ + pos$VZ + pos$LID + pos$VNW + pos$WW + pos$BW + pos$TSW




calcform <- function(formal,informal,name){
  total_pos = rowSums(pos[, formal, drop=FALSE]) + rowSums(pos[, informal, drop=FALSE]) 
  formalitydf[name] <<- (
    #formal categories
    (rowSums(pos[, formal,drop=FALSE])/total_pos)*100 
    
    #informal categories
    - (rowSums(pos[, informal, drop=FALSE])/total_pos)*100 
    +100)/2
}

calcform(c(2,4,5,8),c(3,6,7,9),"original") #original
calcform(c(4,5,8),c(2,3,6,7,9),"renkema") #renkema: adjectives informal
formalitydf$renkemaplus10 <- formalitydf$renkema + 10 #to show that renkema does not change a lot in order from the original, 
#but is just slightly lower because adjectives are now considered informal --> formality score drops
calcform(c(2,4,5,8,11),c(3,6,7,9),"original but with special as formal") #original + special formal (less deixis)
calcform(c(2,4,5,8),c(3,6,7,9,10),"original but with punctuation as informal") #original + punctuation informal
calcform(c(2,4,5,8,11),c(3,6,7,9,10),"orig but with punctuation as informal AND special as formal") #original + punctuation informal + special formal (less deixis)


calcform(c(4,5,8),c(7),"afterscatter") #after analyzing the scatter plots using the original formula

install.packages("xtable")
library(xtable)
xtable(formalitydf)

#==================LOOK AT SIGNIFICANT POS-TAGS FOR THE SCORE, USING MODELS======================
library(lme4)
formality_orig <- lm(formality_orig ~ ADJ+BW+LID+ZNW+TSW+VNW+VZ+WW+LET+SPEC+TW+VG, data=posprop)
summary(formality_orig)
qqnorm(resid(formality_orig))
qqline(resid(formality_orig))
hist(resid(formality_orig))
plot(resid(formality_orig))

formality_sig <- lm(formality_orig ~ ADJ+BW+TSW+VNW+WW+LET+SPEC, data=posprop)
summary(formality_sig)
qqnorm(resid(formality_sig))
qqline(resid(formality_sig))
hist(resid(formality_sig))
plot(resid(formality_sig))

formality_sig2 <- glm(formality_orig ~ ADJ+BW+TSW+VNW+WW+LET+SPEC, data=posprop) 
summary(formality_sig2) #no formal features are significant when using glm 
qqnorm(resid(formality_sig2))
qqline(resid(formality_sig2))
hist(resid(formality_sig2))


#Formal: 5 noun, 2 adj, 8 prep, 4 article
#Informal: 7 pronoun, 9 verb, 3 adverb, 6 interjection
#Nothing: 10 let, 11 spec, 12 tw, 13 vg
calcform(c(2),c(3,6,7,9),"afterregression") #after regression analysis: LET AND SPEC NOT INCLUDED BECAUSE ESTIMATE WAS UNCLEAR

#================================================================================================
