#### BINF 6210 - Assignment 1 - Due Friday October 8, 2021 by 5 pm ----
# By Shalvi Chirmade

## FAMILY OF FELIDAE AND THEIR GEOGRAPHIC DIVERSITY

#### 1- INTRODUCTION ----

#I recently became a cat-mom to two lovely Ragdolls. As they have taken over my life, I chose to study their taxonomic group so I could learn more about the diversity of their taxa. The family Felidae is commonly known to the general population as wild cats. It is a slightly controversial family as many experts disagree on the total number of species that resides in this taxa. The range for number of species is from 36 to 41 (Lamberski, 2015)(O'Brien et al, 2007)(Kitchener et al, 2017). As seen in these papers, all members of the Felidae family are called felids. Felids are native to most geographic areas except for the continents of Antarctica and Australia (Lamberski, 2015). In Australia, felids have been physically introduced by humans (Lamberski, 2015). As seen in Lamberski's Taxonomy table, most wild felids are native to Asia, Africa and the Americas. Later on, we will see if this is also true for the specimen distribution in our data set. 

#The evolution of this family started in Asia and has seen a vast amount of divergence based on felid migration (O'Brien et al, 2007). It will be interesting to see the distribution of species across geographical regions as well as the species richness of the family as a whole. Depending on the data and the results to these questions, further investigation will be carried and discussed later on. So, how can we explain the geographic distribution of Felidae?


#### 2- OBTAINING DATA FROM BOLD ----

#The taxonomic group chosen is the family of Felidae. Here is the link to the Public Data Portal in BOLD http://www.boldsystems.org/index.php/Public_SearchTerms?query=Felidae[tax]. As you can see, there are 831 published records and 54 BINs available for the public encompassing 38 species.

#If you need to install any of the packages I will be using for my assignment, please feel free to uncomment the lines you require.
#install.packages("tidyverse")
#install.packages("vegan")
#install.packages("countrycode")
#install.packages("RColorBrewer")

#Load in packages used in this script
library(tidyverse)
library(countrycode)
library(vegan)
library(RColorBrewer)


#I downloaded the data file using the API tool in BOLD. I used the 'Full Data Retrieval' Section for creating this url. The final file used was downloaded on September 26, 2021.
dfFelidae <- read_tsv(file = "http://www.boldsystems.org/index.php/API_Public/combined?taxon=Felidae&format=tsv")
#I redownloaded the file on October 2, 2021 and October 6, 2021 to make sure there was no additional data inputted. There should be 1342 observations in this database.

#Check my current working directory.
getwd()

#Set working directory. For me it is: 
#setwd("~/Documents/MBinf/BINF 6210/Assignments/1")


#I will save my data into my own computer for future reference. The command is commented as I do not want this to happen on your personal computers when this script is run. I also do not want to rewrite my existing file every time I run this script.
#write_tsv(dfFelidae, "FelidaeDataBOLD.tsv")

#While the making of this assignment, I will create the data frame from the tsv file on my computer.
#dfFelidae <- read_tsv(file = "FelidaeDataBOLD.tsv")


#Time to explore our data! Take a look at the dimensions and analytics of this data set. Note the difference in the class of this data frame created in tidyverse. 
class(dfFelidae)
dim(dfFelidae)
summary(dfFelidae)
length(dfFelidae)
names(dfFelidae) #This is useful to have in my terminal while contemplating which columns to choose for my subset data frame. It allows me to explore my options. When I see a column header I want to use, I look at the data frame and inspect the information for that column (as well as the summary R created for me earlier).

#### 3 - ANALYSIS & OBSERVATIONS ----

##As I am investigating the family, Felidae, the hierarchy of every record should remain the same. 
#Make sure every record has the same phylum name, class name, order name and family name. Are there any anomalies? Are there any NAs?
unique(dfFelidae$phylum_name)
unique(dfFelidae$class_name)
unique(dfFelidae$order_name)
unique(dfFelidae$family_name)
#They all have the correct taxonomy hierarchy! There are no NAs in any of the records for these columns.



##How many species are there in total? 
#Comment on the number and compare to the literature. 
length(unique(dfFelidae$species_name))
#The number of unique species, shown on BOLD, was 38. Using this command, we receive 39. Let's investigate; here is a list of the unique species.
unique.species <- unique(dfFelidae$species_name)
unique.species 
#This is due to the NA. In the introduction, we talked about the controversy over the total number of felid species; this data gives us a value in the range we expect.

#Which records contain NA in species?
print(dfFelidae$processid[is.na(dfFelidae$species_name)]) 
#I'm going to make this record into its own data frame for easier examination.
BDNGS019_15 <- dfFelidae[dfFelidae$processid == "BDNGS019-15" , ]
dim(BDNGS019_15) #check dimensions
#I was very curious about this specimen, so I went onto the BOLD database and entered the processid in a record search. BOLD tells me that the sequence of this specimen was not barcode compliant, and does not have an associated BIN. After accessing the specimen page, most of the data viewed is in my dfFelidae data frame. However, in the data frame, all the columns for notes have a NA value but on the specimen page, I can see a few notes. The collection note says "blowfly-derived DNA". From this information, I can contemplate the reason behind the record not acquiring a BIN or species name. As the sample was derived from a blowfly, we can guess that this sample was found after assessing the gut-microbiome of the fly or simply rogue DNA found on the body of the fly. Due to the minuscule amount of DNA they must have found, it is understandable that the nucleotide sequences were mostly N's which would make it hard to confirm the species of the specimen as well as forming a BIN.
rm(BDNGS019_15) #I will not be using this data frame again in this assignment.



##How many records are in the database per species?
sort(table(dfFelidae$species_name), decreasing = TRUE)

#Select the top two species in the database and analyze why these species are studied the most.
sort(table(dfFelidae$species_name), decreasing = TRUE)[1:2]
#Lynx pardinus and Pathera tigris are amongst the most endangered species of felids(Johnson, 2011)(Dacres, 2007). Their common names are Iberian lynx and tigers. They are considered extinct in many regions of the world where they were once populous. Endangered species are generally widely studied to better understand conservation, this could be why they were the top two species with the highest records.

#I want to take a look at where these specimens are stored and if there is a particular institute that studies their endangerment.
dfEndagered <- dfFelidae %>%
  filter(species_name == "Lynx pardinus" | species_name == "Panthera tigris")

unique(dfEndagered$institution_storing)
#They are from zoos and museums along with GenBank.
rm(dfEndagered) #I will not be using this data frame again in this assignment.



##How many unique BINs are there in the database? Compare to number of species, the total database and comment on NAs.
length(unique(dfFelidae$bin_uri)) #55

#How many records with NAs?
length(dfFelidae$bin_uri[is.na(dfFelidae$bin_uri)]) #base R method
dfFelidae %>%
  count(is.na(bin_uri)) #tidyverse method
#I found the output from count() interesting, it is handy to know both true and false at the same time for a command. It allows for easier analysis.

#Are most of these mined from GenBank?
InstitutionStoring <- dfFelidae$institution_storing[is.na(dfFelidae$bin_uri)]
length(grep("GenBank", InstitutionStoring)) #23
#Ratio of records mined from GenBank to the whole data set.
length(grep("GenBank", InstitutionStoring)) / length(InstitutionStoring) #0.32
#You can see most of the NAs are not mined from GenBank but from different institutions across the world, such as museums and biodiversity centers. The cause of unspecified BINs can be due to poor samples or insufficient sequencing.

#The number of unique BINs is higher than the number of species found in this database. This is the ratio of BINs to species. 
length(unique(dfFelidae$bin_uri)) / length(unique(dfFelidae$species_name)) #1.41
#This suggests that the BIN algorithm created a larger number of clusters during analysis than the actual number of species. This can occur due to a greater number of split patterns associated between BINs and species. Intervening data points can also cause sequences in the BIN algorithm to cluster together. BIN richness is usually correlated to species richness; we can see this in the Felidae database. It tells you about the high level of diversity between each species.

##How many BINs don't have associated species?
dfFelidae %>%
  filter(!is.na(bin_uri)) %>%
  filter(is.na(species_name))%>%
  count()
#The answer is 0, which confirms my explanation earlier of why there is a higher number of unique BINs over the number of species.



##How many unique countries are there in this database?
length(unique(dfFelidae$country)) #44
#Too many to analyze! I will later break it down to regions and continents for easier analysis of this family's global diversity.



##How many records do not have their sampling country specified? Compare to total.
sum(is.na(dfFelidae$country)) #936
sum(is.na(dfFelidae$country)) / length(dfFelidae$country) #0.7
#Only about 30% of our data has country specified, which means our geographical analysis will not be completely accurate due to a high amount of incomplete records. We have already discussed the reasons behind multiple NA values.



##How many unspecified country records have inputs in their corresponding latitude and longitude cells? If I find any, I will correspond each of those records to my new region and continent columns.
dfFelidae %>%
  filter(is.na(country)) %>%
  filter(!is.na(lon) & !is.na(lat)) #0
#There are none! This tells us that every record with latitude and longitude coordinates, has a country specified. This is good news for my further analysis.



##Here is the spread of records per country in the database. I have refrained from sorting the column by number of records as I want to easily locate country names.
dfCountries <- dfFelidae %>%
  count(country) %>%
  filter(!is.na(country)) #43
#Note that Australia is not amongst the list of countries. This solidifies our findings from the introduction that Felidae is not native to the country/continent of Australia; felids were introduced to the area by humans.

#A little more information about the record with the country, Exception - Culture.
dfEC <- dfFelidae %>%
  filter(str_detect(country, "Culture"))
#After investigating on BOLD with the processID, I could not find an explanation of what this means. I'm choosing to remove it from my final subset data frame. Please see the section where I create my subset data frame for the code.
#Here, I had made a circular barplot to showcase the different countries and their number of records. However, I decided to remove it as the graph was not as descriptive as I would have liked. Due to few records with associated countries, the graph looked almost non-existent; most bars were minuscule in comparison to a few countries that had over a hundred records. Later on, I create a graph displaying this using regions of the world which I think is easier to interpret.
rm(dfEC, dfCountries) #I will not be using these data frames again in this assignment. 



##As our breakdown of the database continues, we can see that most elements in the data frame have NA values. This shows that the family of Felidae is not deeply studied. This group of animals are considered a threatened taxa; a large number of species are on the verge of extinction (Brodie, 2009). Many of these species have minimal publications, Brodie identified 14 species. A greater amount of research is required to continue the preservation and conservation of this family.



##Where have all these records been stored? Are they mostly mined from GenBank? Are there any from the Biodiversity Institute in Guelph? How many records come from a museum?
sum(is.na(dfFelidae$institution_storing)) #1
#There is only 1 record without an institution storing input. That's pretty impressive!

#How many of these records have been mined from GenBank? It could explain the large amount of incomplete columns we have been noticing.
dfFelidae%>%
  filter(str_detect(institution_storing, "GenBank")) %>%
  count() #1053
#As we have deduced, most of the records in this database have been obtained from GenBank.

#Are they any from the Biodiversity Institute in Guelph?
unique(dfFelidae$institution_storing)
#Here I found "Centre of Biodiversity Genomics" which is a part of the Biodiversity Institute of Guelph. I also found multiple entries where samples were sequenced at the Biodiversity Institute.

dfBiodiversity <- dfFelidae%>%
  filter(str_detect(sequencing_centers, "Biodiversity Institute")) %>%
  filter(str_detect(institution_storing, "Centre for Biodiversity Genomics")) #3
#There are three records! There are also quite a few other samples that have been sent from across the world to the Biodiversity Institute for sequencing as well. Hence I had to add two different filters for this information.
rm(dfBiodiversity) #I will not be using this data frame again in this assignment.

#How many samples were obtained from Museums? This is just out of curiosity; I'm always amazed at the variety of samples Museums can hold!
length(grep("Museum", dfFelidae$institution_storing)) #43



##Create smaller data frame for further analysis. 
dfSubsetFelidae <- dfFelidae[ , c("processid", "genus_name", "species_name", "country")]
#I chose these four columns are they pertain to the rest of my assignment. My question is centered around the geographical distribution of species and species richness. As there are no records with latitude and longitude values where country has a NA, I will not be adding extra records.

#Remove all the records that have NA in country, genus and species. Remove record with "Exception - Culture" in country (explanation given earlier).
dfSubsetFelidae <- dfSubsetFelidae %>%
  filter(!is.na(species_name)) %>%
  filter(!is.na(genus_name)) %>%
  filter(!is.na(country)) %>%
  filter(!str_detect(country, "Culture"))

#Check to make sure this record was removed.
grep("Culture", dfSubsetFelidae$country) #0
sort(unique(dfSubsetFelidae$country))

#Check the dimensions of our data frame.
dim(dfSubsetFelidae)



##For every record, I will be associating the country to its appropriate continent and region of the world. This is done by using the package, countrycode. You can read more about it here https://cran.r-project.org/web/packages/countrycode/countrycode.pdf. This will allow me to add a column for each region and continent. The package groups the continents North America and South America, so I decided to use regions as well for a better understanding of the geographic spread. The regions and continents from this package are defined by the World Bank Development Indicators.
dfSubsetFelidae$region <- countrycode(sourcevar = dfSubsetFelidae$country, origin = "country.name", destination = "region")
dfSubsetFelidae$continent <- countrycode(sourcevar = dfSubsetFelidae$country, origin = "country.name", destination = "continent")
#I cannot tell you how many hours I spent trying to figure this out! It was definitely a huge learning curve for me!
#As countrycode requires an input of a character vector, I had to take time to fully comprehend the difference between a data frame in base R and a tibble in tidyverse. According to my error messages, I had to use [[ ]] or $ to specify the column of interest and I could not use [ , "country"]. When you index from a data frame, you get a vector; when you index from a tibble, your outcome is also a tibble. After reading this source, https://tibble.tidyverse.org/reference/subsetting.html , I understood that using [[ ]] or $ allows a tibble to output a vector.



##In my introduction, I found that the family Felidae originated in Asia. I want to see if there is a higher percentage of unique species from Asia in comparison to the rest of the world. 
#First, how many unique species are there in Asia?
dfSubsetFelidae %>%
  group_by(continent, species_name) %>%
  filter(continent == "Asia") %>%
  distinct(species_name)
#There are 13 unique species in Asia in this subset data frame.

#How many unique species do we have now after we've created our subset tibble.
dfSubsetFelidae %>%
  distinct(species_name) #30
#Asia has almost half of the total unique species in this subset tibble. As we saw earlier, the total number of unique species in the data frame from BOLD, was 38. From this result, I can say that there is a high species variety sampled in Asia.



##According to Lamberski's Taxonomy table, most wild felids are native to Asia, Africa and the Americas. Let's see if the records have a higher ratio from these regions compared to the total regions.
dfAAA <- dfSubsetFelidae %>%
  filter(continent == "Asia" | continent == "Africa" | continent == "Americas")
#There are 387 records in this subset data frame from these three regions, compared to the total of 404. We can see that 0.97% of the records in this data frame are from these three regions! The records are observationally similar to Lamberski's Taxonomy table. This is shown visually below using ggplot.
rm(dfAAA) #I will not be using this data frame again in this assignment.



##As my subset data frame is quite small, I've decided to use a stacked barplot to visualize the diversity of species by region. As there as 30 species in this smaller data frame, I will be using the genus name instead of the species name to analyze diversity.

#First, I am creating a data frame that counts the number of each species by country.
dfSpeciesbyRegion <- dfSubsetFelidae %>%
  group_by(region, genus_name, species_name) %>%
  count(species_name)

#Create my color palette by adding extra colors to the palettes available. I have chosen a colorblind-friendly brewer palette according to https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/ .
colors <- colorRampPalette(brewer.pal(11, "BrBG"))(14)

#Create a graph to visualize diversity of genus by region.
ggplot(dfSpeciesbyRegion, aes(x = reorder(region, desc(region)), y = n, fill = genus_name)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) + 
  ggtitle("Diversity of Felidae by Region") + 
  xlab("Regions of the World") + 
  ylab("Number of Records") + 
  labs(fill = "Genus Name") +
  scale_x_discrete(labels = function(xaxis) str_wrap(xaxis, width = 15)) +
  coord_flip() + 
  scale_fill_manual(values = colors) +
  theme_bw()

#I had to use genus instead of species because the graph for species was incomprehensible. Genus still allows me to analyse the diversity of felids within each region. As we can see, our graph confirms an extensive amount of research is conducted on Panthera in all regions of the world. We previously also learned that Lynx was a highly studied taxa, however, this is not showcased in the barplot. This is because most records of the Lynx taxa did not have associated countries specified and were excluded from the data set used to create this graph. The difference in the genus studied per region can be based on the native species of felids or the samples available in local museums and zoos. There is a definite difference seen in the diversity found in each region which corresponds to the research conducted by Kitchener et al. and aids in answering our main question.



##Create an individual-based rarefaction curve to assess species richness by region in this data set. The curve is graphed by increments of random sampling of these records to display the species diversity sampled from BOLD. We should see a variety of plots due to the diversity of species sampled from each region. As we saw in the barplot earlier, some regions have a greater variety of species and have a higher sampling quota. This should be visualized in our rarefaction curve.

#I will be using a new subset data frame that counts the number of each species by region.
dfSpeciesCount <- dfSubsetFelidae %>% #new
  group_by(region, species_name) %>%
  count(species_name)
#Check the dimensions of the data frame. 
summary(dfSpeciesCount)
dim(dfSpeciesCount)

#To use the rarecurve function, vegan requires our data to be a community object. This can be achieved by using the function, pivot_wider. It allows us to transpose the data so our rows become columns; physically pivoting the data.
dfPivot <- pivot_wider(data = dfSpeciesCount, names_from  = species_name, values_from = n)
#Check the dimensions of this data frame to make sure it has been achieved.
summary(dfPivot)
dim(dfPivot)

#We can also see a lot of NA values, we have to switch them to the value of 0 before proceeding. This is because rarecurve doesn't understand NA values and the function will report an error.
dfPivot[is.na(dfPivot)] <- 0 

#Make region names into row names so the subsequent database only consists of numerical vectors.
dfPivot <- dfPivot %>%
  remove_rownames %>%
  column_to_rownames(var = "region") 
#Check to see if it has worked.
summary(dfPivot)

#Create a color palette to use for the rarefaction curve. I'm using the same palette as my barplot but I'm reducing the colors to fit the number of regions I have. I used eight even though I have seven regions because it prevents my lightest color from being used (which was hard to see on the plot).
colornames <- brewer.pal(8, "BrBG")
#Check to see if I have created a vector with the hexadecimal color values.
colornames

#Now we will be analyzing our species richness by creating a rarefaction curve.
RichCurve <- rarecurve(dfPivot, xlim = c(0, 170), main = "Felidae Rarefaction Curve by Region", xlab = "Number of Records Sampled", ylab = "Species Richness", lwd = 5, col = colornames, label = FALSE)
legend(x = 125, y = 8, inset = 0.1, legend = rownames(dfPivot), fill = colornames, cex = 0.7, title = "Regions", title.adj = 0.07)

#In this plot, we can see a huge variety of species in each region. Latin America & Caribbean have almost reached a plateau with their species diversity while East Asia & Pacific have just reached their transitional phase. This is understandable as they have the highest number of records in the data set. Middle East & North Africa only have two samples so their curve is minuscule in comparison to the other regions with higher sampling records. The other regions are still in their exponential phase which could be due to low level of records.  This graph shows the need for further research on this family as we can see room for exploration on species diversity in almost every region.



##Create an accumulation curve to assess the diversity of species found in each country. The curve is graphed by increments of countries for the the variety of species sampled. As the number of countries chosen increases, the number of species should start to increase. This is proof that not all species of felids are found in every country of the world. Species vary due to the habitat and optimal climate for each variety of cats.
#I am creating a database that counts the number of each species in each country.
dfSpeciesbyCountry <- dfSubsetFelidae %>%
  group_by(country, species_name) %>%
  count(species_name)

#Once again, we need to prepare our data for the specaccum function in vegan. This is achieved by using the same function as before, pivot_wider, where our rows become columns.
dfPivot2 <- pivot_wider(data = dfSpeciesbyCountry, names_from = species_name, values_from = n)

#Check dimensions to see if the data frame I wanted was created.
dim(dfPivot2)

#We can also see a lot of NA values, we have to switch them with the value of 0 before proceeding. This is because specaccum doesn't understand NA values and the function will report an error.
dfPivot2[is.na(dfPivot2)] <- 0
#Check to see if it has worked by finding values of NA. We should get a count of 0.
grep("NA", dfPivot2) #0

#Specaccum cannot understand character vectors; as we have a column for country names, something needs to be done. As I do not want to lose the country data, I'm going to coerce the tibble to make that column into the row names. This would allow dfPivot2 to only consist of numerical vectors.
dfPivot2 <- dfPivot2 %>%
  remove_rownames %>%
  column_to_rownames(var = "country")

#I can check to see if this worked by using this function. It should output all the countries as the names of the rows.
rownames(dfPivot2)

#Now I can formulate my species accumulation curve.
AccumulationCurve <- specaccum(dfPivot2)

#We can visualize the curve by creating a plot.
plot(AccumulationCurve, xlab = "Countries Sampled", ylab = "Species Richness", main = "Felidae Accumulation Curve", col = "aquamarine3", lwd = 3.5)
#This curve is a good showcase of species diversity for the family of Felidae. It is telling us that as we sample from different countries, the variety of species increases. This confirms my findings from the introduction as climate and topology of a geographical location distinguishes the diversity of felids found. I do not expect this curve to plateau completely as more countries are added, but I do expect a more gradual curve as the number of countries increase due to species diversity of felids across  geographical regions.



##I created a phylogeny of the species of felids found in the public data of BOLD using their interactive tool, TaxonID Tree. If you would like to see the PDF, please let me know and I will send it to you!
#I spent quite a few hours trying to figure out how to make a phylogenetic tree in R, but I think I will wait until we learn Bioconductor and try to do it for a future assignment.



#### 4 - RESULTS AND DISCUSSIONS ----

#As we have seen through this assignment, the family of Felidae is not a deeply studied taxa. Most of the records in the BOLD database are mined from GenBank and are sampled from museums and zoos. A lot of wild felids are difficult to access and there are only a limited amount in zoos due to their endangerment. Most species of felids also have specific geographical locations (Kitchener et al, 2017) which make it difficult for extensive analysis. As we found in our data, there were no records from Australia or Antarctica, this is due to the fact that felids are not native in those continents (Lamberski, 2015). There was an extremely high ratio (97%) of samples from the three regions known to have the highest diversity of felids, Asia, Africa and the Americas (Lamberski, 2015). These geographical locations correlate to the data we analyzed from the barplot as well as the rarefaction curve. There is a high level of diversity seen within each region and it displays the need for further research. Most regions have barely reached the transitional phase in their appropriate rarefaction plot; this shows us that the full capacity of species researched from this family is yet to be explored from every region. I also investigated the felid diversity in Asia due to their evolutionary history (O'Brien et al, 2007); out of the samples with country association, Asia had a diverse amount of unique species studied. The geographic diversity was also noticed in my species accumulation curve. To answer my question, we can explain the geographical distribution of Felidae as notably diverse in each region of the world. The analyses performed clearly showcases a diversity of species across the geographical locations. As more countries begin to study the felids in their area, the accumulation curve will become more gradual. 

#The number of total species controversy unfortunately cannot be investigated using the public data in BOLD. The public data shows 38 unique species while the whole data in BOLD for this family shows 41. Gathering the full data would have been compelling to identify the unique species and compare the findings to the literature. In regards to this, I also would have liked to make a phylogeny tree for better species association. I hope by the end of this course, I will be able to improve on my R and coding skills to achieve this. However, I did learn a lot in the duration of this assignment. I finally understand the logic behind indexing, found ways to improve on my code using Tidyverse and learned to interpret new function documentation. I believe you can see the improvement of my coding style during the length of the analysis and observation section. All in all, this assignment took me on a learning journey for R, BOLD and the family of Felidae. I enjoyed the educational experience and found a new family of taxa that interests me. The conservation of felids is such an important path, I know from now on it will be a topic I frequent more.



#### 5 - REFERENCES AND ACKNOWLEDGMENTS ----

##In the order of references used, useful websites and personal acknowledgments.

#1 - Lamberski N. (2015). Felidae. Fowler's Zoo and Wild Animal Medicine, Volume 8, 467–476. 

#2 - O'Brien, S., & Johnson, W. (2007). The evolution of cats. Genomic paw prints in the DNA of the world's wild cats have clarified the cat family tree and uncovered several remarkable migrations in their past. Scientific American, 297 1, 68-75 .

#3 - Kitchener A. C., Breitenmoser-Würsten Ch., Eizirik E., Gentry A., Werdelin L., Wilting A., Yamaguchi N., Abramov A. V., Christiansen P., DriscollC., Duckworth J. W., Johnson W., Luo S.-J., Meijaard E., O’Donoghue P., Sanderson J., Seymour K., Bruford M., Groves C., Hoffmann M.,Nowell K., Timmons Z. & Tobe S. 2017. A revised taxonomy of the Felidae. The final report of the Cat Classification Task Force of the IUCN/SSC Cat Specialist Group. Cat News Special Issue 11, 80 pp
#Emailed Jacqueline to make sure I could use this reference. She mentioned that because it is peer-reviewed, it is a reliable source.

#4 - Ratnasingham, S. & Hebert, P. D. N. (2007). BOLD: The Barcode of Life Data System (www.barcodinglife.org). Molecular Ecology Notes 7, 355-364.

#5 - Brodie, J.F. (2009). Is research effort allocated efficiently for conservation? Felidae as a global case study. Biodiversity and Conservation, 18, 2927-2939.

#6 - Johnson, C. 2011. "Lynx pardinus" (On-line), Animal Diversity Web. Accessed September 30, 2021 https://animaldiversity.org/accounts/Lynx_pardinus/
#Citation given by website

#7 - Dacres, K. 2007. "Panthera tigris" (On-line), Animal Diversity Web. Accessed September 30, 2021 https://animaldiversity.org/accounts/Panthera_tigris/
#Citation given by website

#8 - Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686

#9 - Arel-Bundock et al., (2018). countrycode: An R package to convert country names and country codes. Journal of Open Source Software, 3(28), 848

#10 - Jari Oksanen, F. Guillaume Blanchet, Michael Friendly, Roeland Kindt, Pierre Legendre, Dan McGlinn, Peter R. Minchin, R. B. O'Hara, Gavin L. Simpson, Peter Solymos, M. Henry H. Stevens, Eduard Szoecs and Helene Wagner (2020). vegan: Community Ecology Package. R package version 2.5-7

#11 -  Kirill Müller, Hadley Wickham. October 1, 2021. Subsetting Tibbles. https://tibble.tidyverse.org/reference/subsetting.html

#12 -  R graph gallery, 2018. Accessed Septeber 28, 2021. R Graph Gellery. https://www.r-graph-gallery.com/all-graphs.html

#13 - PatrickT, 2014. Accessed October 1, 2021. Wrap long axis labels via labeller=label_wrap in ggplot2. https://stackoverflow.com/questions/21878974/wrap-long-axis-labels-via-labeller-label-wrap-in-ggplot2

#14 - RCoder, 2021. Accessed October 4, 2021. Add legend to a plot in R. https://r-coder.com/add-legend-r/

#15 - Alboukadel, 2018. Accessed October 4, 2021. The A-Z of RColorbrewer Palette. https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/

#16 - Alex Egoshin, 2021. Accessed September 28, 2021. Distribution of Big Cats mapped. https://vividmaps.com/big-cats/
#He is a GIS specialist and I contacted him through his webpage to discuss his data references.

#17 - Nykole Crevits 
#Helped me navigate through BOLD and understand the API tool. We went through my data set together and distinguished the prominent columns I could use for my assignment.
#Discussed the reason behind the NA species for the blowfly-derived DNA.
#Idea for using a stacked barplot to show genus variation in each region. Helped with the understanding of the functions reorder, legend and position_stack.
#Idea for using multiple rarefaction curves in one plot to show diversity of species in each region. Helped with the alignment of the plot legend.

#18 - Patricia Balbon
#Helped me remove a row by using indexing [ ].
#Helped me work through trying to create a phylogeny tree in R. I did not succeed but she did (it looks great)!

#19 - My study group --> Nykole Crevits, Patricia Balbon and Emily Maier
#Continuous guidance and support while working through this assignment.

#20 - Eva Merk - A data analyst and a friend I met through my cat's Instagram. She has helped me with understanding Tidyverse and continual support through my programming journey.

#21 - Dr Sarah Adamowicz
#Descriptive lectures and detailed scripts which helped guide my understanding in R and find a pathway through this assignment.

#22 - Jacqueline May - Aiding in understanding my ggplot error message.