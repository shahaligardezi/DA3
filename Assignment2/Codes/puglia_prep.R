# SET UP ------------------------------------------------------------------
#
# CLEAR MEMORY
rm(list=ls())

library(tidyverse)
library(stargazer)
library(Hmisc)

# data used
# download the data from https://github.com/Viki-Meszaros/CEU-Data-Analysis-3/blob/main/Assignment_1/Data/Clean/airbnb_paris_cleaned.rds
# then set your path to be able to run the code (I can't link it directly as it is too big, so you have to download)


path <- "/Users/shahali/Documents/winter_semester/DA3/Assignments/Assignment2/"

# set data dir, load theme and functions
source(paste0(path, "da_helper_functions.R"))
source(paste0(path, "theme_bg.R"))

data_in <- paste0(path,"Data/Clean/")
data_out <- paste0(path,"Data/Clean/")
output <- paste0(path,"Output/")


#-------------------------------------------------------
# Import data
df <- read.csv(paste(data_in,"puglia_cleaned.csv", sep = ""))


# FILTER DATA TO ACTUAL CASE ----------------------------------------------

# check for different property types
types <- df %>% group_by(property_type) %>% 
  summarise(number = n()) %>% 
  arrange(.,-number)


# keep if property type is Apartment
df <- df %>%
  filter( property_type %in% c('Entire home/apt', 'Entire serviced apartment', 'Entire loft','Entire condominium (condo)') )


# keep if accommodates 2-6 people
df <- df[df$accommodates >= 2 & df$accommodates <= 6,]

glimpse(df)

# CLEANE VARIABLES AND CREATE WORKFILE ------------------------------------------------
###############################
#### FACTORS
#

# Property type as factor
df %>% 
  group_by(property_type) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

df$property_type <- word(df$property_type, -1) 

df <- df %>% 
  mutate( f_property_type = factor(property_type))


df[] <- lapply(df, function(x) gsub("[][(),]", "", x))
df$f_property_type

#Room type as factor 

#Checking how many room types are there? 2199 total 
df %>% 
   group_by(room_type) %>% 
   summarise(cnt = n())

unique(df$room_type)


#Creating a Factor Room type as factor and calling it as f_room_type 
df <- df %>%
  mutate(f_room_type = factor(room_type))

####################
#Neighbourhood


unique(df$neighbourhood_cleansed)
typeof(df$neighbourhood_cleansed)

df %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(cnt = n())


#entering unique neighbourhood names 
df <- df %>%
  mutate(f_neighbourhood_cleansed = factor(neighbourhood_cleansed, levels = c("Vico del Gargano", "Vieste", "Gallipoli","Monopoli",
                                                                               "Mesagne", "Uggiano la Chiesa","Alberobello", "Lecce" ,               
                                                                               "Brindisi", "Bari" ,"Parabita","Castrignano del Capo" ,
                                                                              "Nard" , "Gagliano del Capo" , "Carovigno", "Porto Cesareo",        
                                                                             "Galatina" , "Leverano" ,  "Matino" , "Isole Tremiti"  ,      
                                                                              "Ugento" ,  "Otranto", "Polignano a Mare" ,  "Morciano di Leuca" ,   
                                                                               "Specchia" , "Ostuni",  "Arnesano",  "Scorrano" ,            
                                                                               "Salve" , "Cassano delle Murge" , "Sannicandro di Bari", "Lizzanello",         
                                                                               "Martina Franca" ,  "Ruvo di Puglia" , "Adelfia" ,  "Trani"    ,            
                                                                               "Santa Cesarea Terme" ,  "Casarano" ,  "Cutrofiano",  "Gioia del Colle" ,     
                                                                               "Campi Salentina" ,  "Manfredonia" ,  "Galatone",  "Bisceglie" ,           
                                                                               "Giovinazzo"  ,  "Maruggio" , "Taranto" ,  "Botrugno" ,            
                                                                               "Ceglie Messapica" ,  "Manduria" ,  "Andria" ,  "Peschici" ,            
                                                                               "Fasano", "Tiggiano" ,  "Secl" ,  "Pat",              
                                                                               "Melendugno"  , "Alliste" ,  "Terlizzi" ,  "Spongano" , 
                                                                               "Foggia" ,  "Presicce",  "Locorotondo" ,  "Sternatia"  ,        
                                                                               "Rodi Garganico" ,  "Bitonto" ,"Neviano" , "Diso"  ,              
                                                                               "San Pietro Vernotico" , "Taviano" , "Zollino"  , "Sannicola" ,           
                                                                               "Corato" , "Alezio"  , "Margherita di Savoia" , "Carpignano Salentino" ,
                                                                               "Capurso", "Lucera" , "Oria" ,  "Francavilla Fontana",  
                                                                               "Lesina", "Surbo", "Alessano",  "Putignano"  ,          
                                                                               "Castro" ,  "Grottaglie", "Veglie"  ,   "Aradeo"   ,            
                                                                               "Castellaneta" , "Acquaviva delle Fonti" ,"Lequile"  ,"Pulsano"  ,            
                                                                               "Cursi" , "Squinzano" , "San Giovanni Rotondo" , "Casamassima" ,       
                                                                               "Troia" , "Martano" , "San Severo",  "Turi" ,       
                                                                               "Ginosa"  ,  "Melissano" , "Calimera" , "Tricase",          
                                                                               "Mattinata" ,  "Palagiano",  "Torchiarolo", "Soleto",       
                                                                               "Maglie" ,"Racale" , "Giuggianello" ,  "Molfetta",       
                                                                               "Cerignola"  ,"Canosa di Puglia", "Ischitella" ,"San Giorgio Ionico"  , 
                                                                               "Pietramontecorvino" ,"Poggiardo" ,"Palmariggi","Andrano"  ,           
                                                                               "Altamura","Bovino", "Laterza"  ,"Torricella" ,          
                                                                               "Lizzano"  ,"Massafra" , "Cavallino", "Minervino Murge"  ,    
                                                                               "Mottola" , "Surano" , "Acquarica del Capo" , "Deliceto"  ,           
                                                                               "Castellana Grotte" , "Noicattaro" , "Sogliano Cavour" , "Cisternino" ,          
                                                                               "Barletta" , "Modugno","Gravina in Puglia", "Castrignano de' Greci",
                                                                               "Monteroni di Lecce" ,"Vernole"  , "Leporano", "Serracapriola" ,    
                                                                               "Zapponeta"  , "Melpignano" ,"Tuglie"  ,"Conversano"  ,         
                                                                               "Rutigliano"  ,"Crispiano"  , "Ruffano"   ,"Sava"    ,             
                                                                               "Monte Sant'Angelo"  , "Martignano"  ,"Palagianello"  , "Trepuzzi"  ,           
                                                                               "San Marco in Lamis"  , "Poggiorsini" , "Montesano Salentino"  , "Novoli"  ,             
                                                                               "Ortelle" , "San Vito dei Normanni", "Cagnano Varano"  , "Latiano"  ,            
                                                                               "Corigliano d'Otranto" , "Giurdignano"  ,"San Michele Salentino", "Mola di Bari",         
                                                                               "Minervino di Lecce"  , "Copertino" , "Taurisano","Cellino San Marco", 
                                                                               "San Pietro in Lama" ,"Corsano"  , "Poggio Imperiale" )))


#Realising that I need to use grouped neighborhood

unique(df$neighbourhood_group_cleansed)

df <- df %>%
  mutate(f_neighbourhood_group_cleansed = factor(neighbourhood_group_cleansed, levels = c("Foggia", "Lecce","Bari","Brindisi","Taranto" ,
                                                                                          "Barletta-Andria-Trani")))

#Response time

#total number of NAs in response time: 40% are NAs. As per book page:397 if NAs are 
#random and we can expect to have random NAs in Live Data then we can drop it

df %>% 
  group_by(host_response_time) %>% 
  summarise(cnt = n()) %>%  
  arrange(-cnt)


# get host_response_time as factors
df <- df %>% 
  mutate(f_host_response_time = factor(host_response_time, levels = c( "within an hour",  "within a few hours",
                                                                       "within a day", "a few days or more")))


#checking response rate
df %>% 
  group_by(host_response_time) %>% 
  summarise(cnt = n()) %>%  
  arrange(-cnt)


###############################
#### NUMERIC VARIABLES
#
## Create Numerical variables

#price is in Euros:
df <- df %>%
  mutate( p_host_response_rate = as.numeric(host_response_rate),
          p_host_acceptance_rate = as.numeric(host_acceptance_rate))
          

# clean number of bathrooms
df <- df %>% rename(bathrooms = bathrooms_text)


# get the number of baths from bathroom_text
df$bathrooms <- as.numeric(gsub("[^0-9.-]", "", gsub("half", 0.5, df$bathrooms, ignore.case = T)))

unique(df$bathrooms)

#dropping property with no bathroom at all
df <-  subset(df, bathrooms != 0 & bathrooms != "NA")

df %>% 
  group_by(bathrooms) %>% 
  summarise(cnt = n()) %>%  
  arrange(-cnt)


# add new numeric columns from certain columns
numericals <- c("accommodates","bathrooms", "bedrooms", "beds", "review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights", "availability_365")

df <- df %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- df %>%
  select(ends_with("_n")) %>%
  names()

nnames_i <- match(nnames, colnames(df))
colnames(df)[nnames_i] <- paste0("n_", numericals)


#create days since first review
df <- df %>%
  mutate(
    n_days_sincefirst = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))

df %>% 
  group_by(n_days_sincefirst) %>% 
  summarise(cnt = n()) %>%  
  arrange(-cnt)


df <- df %>%
  mutate(
    n_days_sincelast = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(last_review ,format="%Y-%m-%d")))


df %>% 
  group_by(n_days_sincefirst) %>% 
  summarise(cnt = n()) %>%  
  arrange(-cnt)



###############################
#### DUMMY VARIABLES
#
# create dummy vars
dummies <- c(names(df)[seq(50,134)],"instant_bookable","has_availability","host_is_superhost", "host_identity_verified" )


df <- df %>%
  mutate_at(vars(dummies), funs("d"= (.)))

# rename columns
dnames <- df %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(df))
colnames(df)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))



# CREATE WORK FILE --------------------------------------------------------

# keep columns if contain d_, n_, f_, p_, usd_ and some others
df <- df %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id,
         neighbourhood_cleansed,room_type,property_type)


write_csv(df, paste0(data_out, "puglia_workfile.csv"))
saveRDS(df, paste0(data_out, "puglia_workfile.rds"))

library(skimr)


# CLEANING VALUES -------------------------------------------------------------------------


#####################
### looking at price ###
#####################


#dealing with price
summary(df$price)
describe(df$price)
datasummary_skim(df)


df$price  <- as.numeric(df$price)

df %>% filter(price >= 500) %>% 
  summarise(cnt = n()) %>%
  arrange(-cnt)

#dropping EV values
df <-   df %>% filter(price <= 500)

# with price info only
df <- df %>%
  drop_na(price)

# Price Distribution

price_hist <- ggplot(df, aes( x = price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),fill = "cyan3", color = "black") +
  theme_bw() +
  scale_y_continuous(labels = label_percent()) +
  ylab("Percent") +
  xlab("Price (Euros)")
price_hist

ln_price_hist <- ggplot(df, aes(x = log(price))) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),fill = "cyan3", color = "black") +
  theme_bw() +
  scale_y_continuous(labels = label_percent()) +
  ylab("Percent") +
  xlab("ln(Price, Euros)")
ln_price_hist

price_hist_grid <- ggarrange(
  price_hist,
  ln_price_hist,
  nrow = 1)


annotate_figure(price_hist_grid,bottom =
                  text_grob("Note: Apartments with 2-6 accommodation capacity. Histogram without extreme values (price < 500 Euros)"))


###############################
# Handling missing values #
###############################


# where do we have missing values now?
to_filter <- sapply(df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


#f_host_response_time   p_host_response_rate    p_host_acceptance_rate             n_bedrooms 
#875                    875                        290                               101 
#n_beds        n_review_scores_rating     n_reviews_per_month      n_days_sincefirst 
#71                    823                      823                    823 
#n_days_sincelast    d_reviews_per_month  
#823                    823                     


describe(df$n_accommodates)
describe(df$n_beds)


#drop columns when many missing not important

df$f_host_response_time <- NULL
df$p_host_acceptance_rate <- NULL
df$p_host_response_rate <- NULL
df$n_days_sincefirst <- NULL
df$d_reviews_per_month <-  NULL


# 3 Imputation

df <- df %>%
  mutate(
   
    n_bedrooms = ifelse(is.na(n_bedrooms), n_accommodates %% 2, n_bedrooms),
    
    n_beds = ifelse(is.na(n_beds), round(n_accommodates / 1.5), n_beds), #assume that 1 bed corresponds to about 1.5 accommodates
    n_beds = ifelse(n_beds == 0, round(n_accommodates / 1.5), n_beds), #assume that 1 bed corresponds to about 1.5 accommodates
    
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month),
    
    flag_days_sincelast=ifelse(is.na(n_days_sincelast),1, 0),
    n_days_sincelast =  ifelse(is.na(n_days_sincelast), median(n_days_sincelast, na.rm = T), n_days_sincelast),
    
    n_bathrooms =   ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms)
    
  )



################################################
# look at some key variable &  functional form #
################################################

## n_accomodates:

df%>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

price_vs_accommodates <- ggplot(data = df, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, colour=color[3], shape=16)+
  ylim(0,500)+
  xlim(0,7)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour=color[1], se=FALSE)+
  theme_bg()
price_vs_accommodates



############################
## n_bathrooms
ggplot(df, aes(n_bathrooms)) +
  geom_histogram(binwidth = 0.5, fill = "cyan4", color = "white", alpha = 0.8) +
  xlab("N of bathrooms") +
  theme_classic()

df %>%
  group_by(n_bathrooms) %>%
  summarise(mean_price = mean(price), n = n())

# check number of baths for different number of accommodates
df %>% 
  group_by(n_accommodates) %>% 
  summarise(num_baths = mean(n_bathrooms, na.rm = T), min_baths = min(n_bathrooms, na.rm = T), max_baths = max(n_bathrooms, na.rm = T))

#### I will create pooled categories -> 1 and 2
#### Already imputed 1  for missing values 

# Pool accommodations with 0,1,2,7 bathrooms (already added)
df <- df %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0,2,7), labels=c(1,2), right = F) )


df %>% 
  group_by(f_bathroom) %>% 
  summarise(cnt = n()) 


############################
## n_bedrooms

ggplot(df, aes(n_bedrooms)) +
  geom_histogram(binwidth = 0.5, fill = "cyan4", color = "white", alpha = 0.8, size = 0.25) +
  xlab("N of bedrooms") +
  theme_classic()

df %>%
  group_by(n_bedrooms) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# check number of bedrooms for different number of accommodates
df %>% 
  group_by(n_accommodates) %>% 
  summarise(num_bedrooms = mean(n_bedrooms, na.rm = T), min_bedrooms = min(n_bedrooms, na.rm = T), max_bedrooms = max(n_beds, na.rm = T))

#Already imputed the value based on logical assumption 2 people will in 1 bedroom 

# Pool accomomdations with 1,2,3,6 bedrooms
df <- df %>%
  mutate(f_bedroom = cut(n_bedrooms, c(0,1,4,7), labels=c(1,2,3), right = F) )

df %>% 
  group_by(f_bedroom) %>% 
  summarise(cnt = n()) 


############################
# n_beds

ggplot(df, aes(n_beds)) +
  geom_histogram( fill = "cyan4", color = "white", alpha = 0.8, size = 0.25) +
  xlab("N of beds") +
  theme_classic()


#taking log for beds
df <-  df %>% mutate(ln_bed = log(n_beds))
# no need of log beds


df %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# check number of beds for different number of accommodates
df %>% 
  group_by(n_accommodates) %>% 
  summarise(num_beds = mean(n_beds, na.rm = T), min_beds = min(n_beds, na.rm = T), max_beds = max(n_beds, na.rm = T))


#Already imputed n_accommodates %% 2 rounded to whole for missing value


# Pool accomomdations with 1,2,3,6 beds
df <- df %>%
  mutate(f_beds = cut(n_beds, c(0,3,6,11), labels=c(1,2,3), right = F) )

df %>% 
  group_by(f_beds) %>% 
  summarise(cnt = n()) %>%  
  arrange(-cnt)


###########################
## n_review_scores_rating
ggplot(data = df, aes(x=n_review_scores_rating , y=price)) +
  geom_point(size=1.5, colour="cyan4", shape=16, alpha=0.6) +
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_classic()

#describe
describe(df$n_review_scores_rating)

#check
df %>% 
  group_by(n_review_scores_rating) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

# Pool num of reviews to 3 categories:  (0 -1 / )
df <- df %>%
  mutate(f_review_scores_rating = cut(n_review_scores_rating, c(0,1,4,6), labels=c(1,2,3), right = F))

#check
df %>% 
  group_by(f_review_scores_rating) %>% 
  summarise(cnt = n())

############################
## n_number_of_reviews
df %>%
  filter(n_number_of_reviews <200) %>% 
  ggplot(aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = "cyan4", color = "white", alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_classic()

# I will also create pools for reviews ->  none, 1-36 and >36
describe(df$n_number_of_reviews)

df %>% 
  group_by(n_number_of_reviews) %>% 
  summarise(cnt = n())

# Pool num of reviews to 3 categories: none, 1-36 and >36
df <- df %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,36, max(df$n_number_of_reviews)+1 ), labels=c(1,2,3), right = F))

df %>% 
  group_by(f_number_of_reviews) %>% 
  summarise(cnt = n())

############################
## n_minimum_nights
df %>% 
  group_by(n_minimum_nights) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)


ggplot(df, aes(n_minimum_nights)) +
  geom_histogram( fill = "cyan4", color = "white", alpha = 0.8, size = 0.25, binwidth = 1) +
  xlim(0,50)+
  xlab("N of minimum nights") +
  theme_classic()

describe(df$n_minimum_nights)

# Pool and categorize the number of minimum nights: 1,2,3, 3+
df <- df %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,5,8,max(df$n_minimum_nights)+1), labels=c(1,2,3), right = F))

df %>% 
  group_by(f_minimum_nights) %>% 
  summarise(cnt = n())


############################
## n_days_sincelast
skimr::skim(df$n_days_sincelast)

ggplot(data = df, aes(x=n_days_sincelast , y=price)) +
  geom_point(size=1.5, colour="cyan4", shape=4) +
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Log number of days since last review",y="Log daily price")+
  theme_classic()


################################################
# Rename some dummy variables                  #
################################################


df <- df %>% rename(d_family_friendly = d_have_childrenbabycribhighcornerchang,
                    d_coffee_machine = d_have_o_machineee_machinecoffee,
                    d_free_parking_on_premises = d_have_freeon_premises,
                    d_free_parking_on_street = d_have_freestreet,
                    d_paid_parking_off_premises = d_have_paidoff_premisesselfparkingparking,
                    d_paid_parking_on_premises =  d_have_paidon_premisvalet,
                    d_wifi = d_have_wifiinternet, 
                    d_shampoo_conditioner = d_have_shampooconditioner,
                    d_balcony = d_have_balconyterrace) 



# SAVE ADJUSTED WORKFILE --------------------------------------------------

write_csv(df, paste0(data_out, "puglia_workfile_adj.csv"))
saveRDS(df, paste0(data_out, "puglia_workfile_adj.rds"))

