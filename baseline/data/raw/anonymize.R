path <- getwd()
dta <- read.csv(paste(path,"latest.csv", sep="/"))
path <- strsplit(path, "/raw")[[1]]

## we have 40 duplicates
sum(duplicated(dta$farmer_ID))
### what is the best way to deal with these duplicates?


#These are the inital (random) prices offered:
#dta$P1_pric

#These are the prices offered by the enumerator
#dta$Check2.check.maize.paid.P3_pric_2 -10

#answers to offers
#dta$Check2.check.maize.paid.start_neg 

#these are the price bids by the farmers
##dta$Check2.check.maize.paid.P2_pric

##determine last ask price

##determine maximum bid price
dta$bid <- ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P2_pric_11)),as.numeric(dta$Check2.check.maize.paid.P2_pric_11),
   ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P2_pric_10)),as.numeric(dta$Check2.check.maize.paid.P2_pric_10),
          ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P2_pric_9)),as.numeric(dta$Check2.check.maize.paid.P2_pric_9),
                 ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P2_pric_8)),as.numeric(dta$Check2.check.maize.paid.P2_pric_8),
                        ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P2_pric_7)),as.numeric(dta$Check2.check.maize.paid.P2_pric_7),
                               ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P2_pric_6)),as.numeric(dta$Check2.check.maize.paid.P2_pric_6),
                                      ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P2_pric_5)),as.numeric(dta$Check2.check.maize.paid.P2_pric_5),
                                             ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P2_pric_4)),as.numeric(dta$Check2.check.maize.paid.P2_pric_4),
                                                    ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P2_pric_3)),as.numeric(dta$Check2.check.maize.paid.P2_pric_3),
                                                           ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P2_pric_2)),as.numeric(dta$Check2.check.maize.paid.P2_pric_2),
                                                                  as.numeric(dta$Check2.check.maize.paid.P2_pric)
   ))))))))))

##determine minimum ask price
dta$ask <-      ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P3_pric_10)),as.numeric(dta$Check2.check.maize.paid.P3_pric_10),
                         ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P3_pric_9)),as.numeric(dta$Check2.check.maize.paid.P3_pric_9),
                                ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P3_pric_8)),as.numeric(dta$Check2.check.maize.paid.P3_pric_8),
                                       ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P3_pric_7)),as.numeric(dta$Check2.check.maize.paid.P3_pric_7),
                                              ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P3_pric_6)),as.numeric(dta$Check2.check.maize.paid.P3_pric_6),
                                                     ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P3_pric_5)),as.numeric(dta$Check2.check.maize.paid.P3_pric_5),
                                                            ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P3_pric_4)),as.numeric(dta$Check2.check.maize.paid.P3_pric_4),
                                                                   ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P3_pric_3)),as.numeric(dta$Check2.check.maize.paid.P3_pric_3),
                                                                          ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P3_pric_2)),as.numeric(dta$Check2.check.maize.paid.P3_pric_2),
                                                                                 ifelse(!is.na(as.numeric(dta$Check2.check.maize.paid.P3_pric)),as.numeric(dta$Check2.check.maize.paid.P3_pric),
                                                                                 as.numeric(dta$P1_pric)
                                                                          ))))))))))

###number of rounds after which farmer agrees
dta$rounds <- NA
dta$rounds[dta$Check2.check.maize.paid.start_neg=="Yes"]  <- 1
dta$rounds[dta$Check2.check.maize.paid.start_neg_2=="Yes"] <- 2
dta$rounds[dta$Check2.check.maize.paid.start_neg_3=="Yes"] <- 3
dta$rounds[dta$Check2.check.maize.paid.start_neg_4=="Yes"] <- 4
dta$rounds[dta$Check2.check.maize.paid.start_neg_5=="Yes"] <- 5
dta$rounds[dta$Check2.check.maize.paid.start_neg_6=="Yes"] <- 6
dta$rounds[dta$Check2.check.maize.paid.start_neg_7=="Yes"] <- 7
dta$rounds[dta$Check2.check.maize.paid.start_neg_8=="Yes"] <- 8
dta$rounds[dta$Check2.check.maize.paid.start_neg_9=="Yes"] <- 9
dta$rounds[dta$Check2.check.maize.paid.start_neg_10=="Yes"] <- 10
dta$rounds[dta$Check2.check.maize.paid.start_neg_11=="Yes"] <- 11

### enumerator gender:
# create named vector of enumerator genders
enumerator_genders <- c(Arnold = "Male", 
                        Badru = "Male", 
                       'Businge Penny' = "Female",
                        Buyera = "Male",
                        Humphrey = "Male",
                        Ibanda = "Male",
                        Kabali = "Female",
                        Kalule = "Male",
                        Katumba = "Male",
                        Khaukha = "Male",
                        Kibaale = "Female",
                        Komako = "Male",
                        Lukabya = "Male",
                        Mpalanyi = "Female",
                        Mulabiza = "Female",
                        Muwata = "Male",
                        Nakirya = "Female",
                        Nambi = "Female",
                        Nambozo = "Female",
                        Nandhego = "Female",
                        Nemwa = "Female",
                        Oboth = "Male",
                        Segujja = "Male",
                        Sendaula = "Male",
                        Wasike = "Male",
                        Yiga = "Male")

# create new variable in your data frame using the named vector
dta$enumerator_gender <- enumerator_genders[dta$enumerator]

## create IDs for district, subcounty, village

### create IDs for district, TA and village (district, sub, village)


dta$distID <- NULL
dta$subID <- NULL
dta$vilID <- NULL

i_dist <- 1
for (dist in names(table(dta$district))) {
  print(dist)
  i_sub <- 1
  for (sub in names(table(dta$sub[dta$district==dist]))) {
    print(sub)
    i_village <- 1
    for (village in names(table(dta$village[dta$district == dist & dta$sub == sub]))) {
      print(village)
      dta$vilID[dta$district == dist & dta$sub == sub & dta$village == village] <- i_village
      i_village <- i_village + 1
    }
    dta$subID[dta$district == dist & dta$sub == sub] <- i_sub
    i_sub <- i_sub + 1
  }
  dta$distID[dta$district==dist] <- i_dist
  i_dist <- i_dist + 1
}

dta$distID <- as.numeric(dta$distID)
dta$subID <- as.numeric(dta$subID)
dta$vilID <- as.numeric(dta$vilID)



## drop location and metadata
to_drop <- c("Check2.check.maize.pic",
                                     "Check2.check.maize.pic2",
                                     "Check2.check.maize.pic3",
                                     "Check2.check.maize.gps",
                                     "Check2.check.maize._gps_latitude",
                                     "Check2.check.maize._gps_longitude",
                                     "Check2.check.maize._gps_altitude",
                                     "Check2.check.maize._gps_precision",
                                     "meta.instanceID",
                                     "X_id",
                                     "X_uuid",
                                     "X_submission_time",
                                     "X_date_modified",
                                     "X_tags",
                                     "X_notes",
                                     "X_version",
                                     "X_duration",
                                     "X_submitted_by",
                                     "X_total_media",
                                     "X_media_count",
                                     "X_media_all_received",
                                     "X_xform_id")           
dta <- dta[ , !(names(dta) %in% to_drop)]


## drop location, names and contact details
to_drop <- c("start","end","deviceid","simserial","phonenumber", "subscriberid", "enumerator","district","sub","village",  "Check2.check.maize.plot_no",
             "Check2.check.maize.plot_count",
             "Check2.check.maize.plot.1..plot_num",
             "Check2.check.maize.plot.1..plot_name",
             "Check2.check.maize.plot.2..plot_num",
             "Check2.check.maize.plot.2..plot_name",
             "Check2.check.maize.plot.3..plot_num",
             "Check2.check.maize.plot.3..plot_name",
             "Check2.check.maize.plot.4..plot_num",
             "Check2.check.maize.plot.4..plot_name",
             "Check2.check.maize.plot.5..plot_num",
             "Check2.check.maize.plot.5..plot_name",
             "Check2.check.maize.plot_calc1",
             "Check2.check.maize.plot_calc2",
             "Check2.check.maize.plot_select",
             "Check2.check.maize.plot_select_name", "Check2.check.maize.order1","Check2.check.maize.phone", "Check2.check.maize.phone2", "Check2.check.maize.name_resp", "Check2.check.maize.nick" )     
             dta <- dta[ , !(names(dta) %in% to_drop)]
             

             names(dta) <- sub("Check2.check.maize.", "",names(dta))



write.csv(dta, file = paste(path,"public/baseline.csv", sep="/"), row.names = FALSE)
