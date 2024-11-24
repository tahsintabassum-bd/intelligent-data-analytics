
install.packages("RColorBrewer")
library(RColorBrewer)
library(ggplot2)library(ggplot2)

ggplot(data = mpg)

?mpg
ggplot(data=mpg, aes(y=hwy, x=cyl)) + geom_point()+ labs(title="Highway Miles per Gallon (hwy) vs Number of Cylinders (cyl)")+ theme(plot.title=element_text(hjust=0.5)) 
ggplot(data = mpg, aes(x = class, y = drv)) +
  geom_point() +
  labs(title = "Type of Car (class) vs The Type of Drive Train (drv)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
#colour
ggplot(mpg, aes(x = displ, y = hwy, color = cty)) +
  geom_point()
#size
ggplot(mpg, aes(x = displ, y = hwy, size = cty)) +
  geom_point()
#shape
ggplot(mpg, aes(x = displ, y = hwy, shape = cty)) +
  geom_point()
#multiple Aesthetics plot
ggplot(data=mpg, aes(x=hwy, y=cty)) + geom_point(aes(color=cty, size= hwy))+
  labs(title="")+
  theme(plot.title=element_text(face="bold",hjust=0.5),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))

#varible name

ggplot(data = mpg, aes(x = hwy, y = cty)) + 
  geom_point(aes(colour = displ < 5)) +
  labs(title = "Color Mapping Based on Engine Displacement") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.x = element_text(face = "bold"),

                axis.title.y = element_text(face = "bold"))
#For Faceting 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
#For colour aesthetic 
ggplot(data=mpg, aes(x=displ, y=hwy)) + geom_point(aes(color=class))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point(alpha=1/5,position='jitter') +geom_smooth()+geom_smooth(method="lm",color="Black",se=FALSE)+ facet_grid(. ~ drv)+ labs(title="Highway MPG vs Displacement", x="Displacement",  y="Highway MPG") 

#visualising housing data

#1 ovSalePrice by Neighborhood
ggplot(housing_data, aes(x = reorder(Neighborhood, SalePrice, median), y = SalePrice)) +
  geom_boxplot() +
  coord_flip() +  # Flip coordinates for readability
  labs(title = "Distribution of Sale Prices by Neighborhood", x = "Neighborhood", y = "Sale Price") +
  theme(plot.title=element_text(hjust=0.5)) 
#2 Building type by Overall Quality

ggplot(housing_data, aes(x = BldgType, fill = factor(OverallQual))) +
  geom_bar(position = "fill") +  # Fill option to stack bars
  labs(title = "Overall Quality of Different Building Types", x = "Building Type", y = "Proportion", fill = "Quality Rating") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "PuBu"))(10)) +  # Generate 10 lighter purple-blue colors
  theme_minimal()

#3 heating facility and shape of property impact on price 

ggplot(data = housing_data, aes(x = HeatingQC, y = SalePrice)) + 
  geom_point(aes(fill = LotShape),   # Fill color based on LotShape
             alpha = 0.75,           # Transparency for points
             position = "jitter",    # Jitter to avoid overplotting
             color = "black",        # Black outline for points
             pch = 21,               # Shape with fill (circle with a border)
             size = 3) +             # Size of points
  theme_bw() +
  labs(y = "Price of houses ($)",
       x = "Heating Condition") +
  scale_fill_manual(values = c("Reg" = "blue",  
                               "IR1" = "green", 
                               "IR2" = "yellow", 
                               "IR3" = "red")) + 
  theme(legend.key = element_blank(),
        axis.title = element_text(size = 14)) +
  ggtitle("The Imapct of Heating Facility and Shape of Property on Price") + 
  theme_minimal()
#4 The price of  building type in different zones

ggplot(data = housing_data) + 
  geom_point(mapping = aes(x = BldgType, y = SalePrice, color=YearBuilt)) +
  facet_wrap(~ MSZoning, nrow = 2)+
  labs(title="Price of Different Building Type in Different Zones", x = "Building Type", y = "House Price ($)") + theme(plot.title=element_text(hjust=0.5))


#5 Scatter plot with trend line
ggplot(housing_data, aes(x = GrLivArea, y = SalePrice, color = factor(OverallQual))) +
  geom_point(alpha = 0.8, size = 2) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_fill_brewer(palette = "Set3") + 
  labs(title = "Relationship between Living Area and Sale Price",
       x = "Above Ground Living Area (sq ft)",
       y = "Sale Price ($)",
       color = "Overall Quality") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))   
