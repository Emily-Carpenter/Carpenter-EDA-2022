# Carpenter-EDA-2022
##Introduction
In the 1960’s, the wolf population in Minnesota was only around 400 wolves. Within the past 40 years, thanks to wildlife biologists, the population has soared to an estimated 2,400 in 2020. Making sure populations are stable is a major way to help ecosystems thrive in the wild. However, what happens when humans are part of that ecosystem. Human-wildlife conflict is when encounters between humans and wildlife end in negative results, such as loss of property, livelihoods, or even life. With the rise in population of wolves in the past couple of decades  there has also been a rise in trapping and killing of  the animals to keep people and livestock safe.(Mech D.) What if we could predict where conflict take place in Minnesota between humans and wolves? That is what is done in this exploratory data analysis. By looking at wolf population, human population, and cattle population we hope to find some pattern as to where we should see human wolf conflict in Minnesota.
##Methods
The wolf population data was obtained from the Minnesota Wolf Population update 2020, documented by the Minnesota DNR. The cattle population data was obtained from a survey recorded by the USDA, and the human population data was taken from Minnesota State Demographic Center’s website. Once the data was organized on excel spreadsheets the data was uploaded, combined, and reorganized using RStudio. Continuing the use of RStudio several graphs were created to display the data in order to find correlation. First, scatterplots were made showing wolf to human population, and wolf to cattle population. The second batch came in the form of maps showing density of our data. Using the ‘ubrmrp’ program, we calculated wolf population/miles squared, human population/miles squared, and cattle population/miles squared to get the densities per county for each data set. Once these were calculated, we created maps showing the ratio of densities compared over each other.
##Results
Looking at Figure 1, which shows the wolf population vs the human population in each county. This data showed no correlation between the two populations. Our line of best fit gave no real indication as to which way our data sways. When we created a scatterplot of the wolf population to cattle population it yielded the same results. The scatterplot showed no true correlation with our numbers being all over the board. Looking at the densities of each data point and mapping, those numbers yielded some possible answers. In figure 2, we took the wolf density over the human density and mapped it out. It is shown that in the lighter blue areas in the northern part of the state is where we would expect to see human wolf conflict. In the middle and southern part of the state, that is shown in dark blue, there is a lower chance of seeing human wolf conflict. In figure 3, we mapped out the wolf density to the cattle density and found that there could be a chance of conflict in the northeastern part of the state in Cook and Lake counties. With the rest of the state having virtually no chance for conflict. They gray areas show where we had no data for cattle or wolves.
##Discussion
The results of this exploratory data analysis are very interesting. Within figure 2 we would expect to see more human wolf conflict in the northern counties, or the lighter blue hues of the ratio scale, because the density ratio of wolves to humans is higher than in the southern portion of Minnesota. The further south you go the human population increases, and the wolf population decreases. Also, the wolf's territory range only goes as far south as Chisago county. That is why the southernmost portion of  the map displays zero display of ratio densities.  With the wolf and cattle map, we see the light blue area where we expect to see conflict are only in Cook and Lake counties. Most of the livestock in Minnesota are in the southern part of the state, and since wolves are in the northern part this explains why there is only predicted conflict in the northeastern tip. It is the only area where the two populations meet. The significance of this exploratory data analysis is that these data sets have never been brought together before. Residents' attitudes on wolves are a little controversial depending on if you are a hunter, livestock owner, or just a resident(Schroeder). However, they do agree on a few things, wolves are a danger to pets, livestock, and the white-tailed deer population.  According to the DNR in 2019, wolves killed 74 calves, 11 cows, 2 sheep, and 2 dogs. From 2014 to 2018 the Minnesota Department of Agriculture made annual depredation payment of $150,000 to livestock producers. Residents also agree that maintaining wolves' numbers and range in Minnesota is important(Schroeder). The residents of Minnesota enjoy having the wolf populations around and take pride in the fact that our state has made it a priority. They also care that the population is managed. The DNR has had a firm grip on management as the wolf numbers continue to rise. If the numbers get out of control our data could help find where it would be best to institute a hunting or trapping season for wolves in Minnesota. The data could also help relocate wolves that have attacked livestock. In almost every case when a wolf is relocated it never comes back to the offending area(Michael W. Fox). The research we have done here could assess the dangers of the wolf population getting too high and ensuring the safety of Minnesota's residents.
