### Regression 2 Homework ###
### Elijah Bakam 3/23/2024 ###

# Loading in my prodictve pacifists dataset
load("/Users/elijahbakam/Library/CloudStorage/GoogleDrive-bakam@usc.edu/.shortcut-targets-by-id/19DfrFWKARGNjNlJ0nheNeauY-8DKul1a/412_413 shared AY23-24/Training Data Fall 23/ProductivePacifists_Data.RDATA")

# First step will be to run some regressions (4-8) with different variables that I choose from the dataset
# I want my dependent variable to be tc_MID_dummy_ICOW to show whether a country
# Participates in a militarized interstate dispute over territory in a given year

# Tiem to make my regressions (DV first, then IV + my controls)
r1 <- lm(tc_mid_dummy_ICOW ~ land_oriented_medium_continuous, data=dat)
r2 <- lm(tc_mid_dummy_ICOW ~ land_oriented_medium_continuous + autocracy_BX, data=dat)       
r3 <- lm(tc_mid_dummy_ICOW ~ land_oriented_medium_continuous + autocracy_BX + milex_constant2010us_AFM, data=dat)
r4 <- lm(tc_mid_dummy_ICOW ~ land_oriented_medium_continuous + autocracy_BX + milex_constant2010us_AFM + land_CONT , data=dat)
r5 <- lm(tc_mid_dummy_ICOW ~ land_oriented_medium_continuous + autocracy_BX + milex_constant2010us_AFM + land_CONT + cinc_MC, data=dat)
# I wanted to make regressions that measure the affect of a continuous measure of economic oreitnation on its own
# And then later with the full set of controls going progressively so I can see the affect of each one

# Now its time to make a table using the regressions I made
htmlreg(list(r1, r2, r3, r4, r5), 
        file = "/Users/elijahbakam/Library/CloudStorage/GoogleDrive-bakam@usc.edu/.shortcut-targets-by-id/19DfrFWKARGNjNlJ0nheNeauY-8DKul1a/412_413 shared AY23-24/Elijah Bakam/effects.on.MIDS.html", 
        caption = "Effects on Territorial Militarized Interstate Disputes", 
        caption.above = TRUE, 
        custom.header = list("Economic Orientation" = 1, "Regime Type" = 2, "Military Expenditures" = 3, "Land Borders" = 4, "Military Capabilities" = 5), 
        stars = c(0.001, 0.01, 0.05), 
        custom.note = "Variables come from the replication data of the Productive Pacifists paper published in International Studies Quarterly")

# Now I will create a logged version of my milex variable and use that in a new regression rather than the raw milex variable
dat$logmilex = log(dat$milex_constant2010us_AFM) # this adds a logged milex variable to my dataset

# Now I can just add that variable directly into a new regression model
r6 <- lm(tc_mid_dummy_ICOW ~ land_oriented_medium_continuous + autocracy_BX + logmilex, data=dat) # Now we can compare this regression to the first regression to include milex 

# Next, I'll simply add r6 into my table syntax
htmlreg(list(r1, r2, r3, r4, r5, r6), # add in my new model
        file = "/Users/elijahbakam/Library/CloudStorage/GoogleDrive-bakam@usc.edu/.shortcut-targets-by-id/19DfrFWKARGNjNlJ0nheNeauY-8DKul1a/412_413 shared AY23-24/Elijah Bakam/effects.on.MIDS.html", 
        caption = "Effects on Territorial Militarized Interstate Disputes", 
        caption.above = TRUE, 
        custom.header = list("Economic Orientation" = 1, "Regime Type" = 2, "Military Expenditures" = 3, "Land Borders" = 4, "Military Capabilities" = 5, "Log of Military Expenditures" = 6), # Update names
        stars = c(0.001, 0.01, 0.05), 
        custom.note = "Variables come from the replication data of the Productive Pacifists paper published in International Studies Quarterly")

# Next, I'm going to add country fixed effects for one of my regression models, I'll choose r5
# We're going to make a new dataset with a country dummy variable 
dat3 <- dat %>%
  mutate(country_dummies = factor(gwno))
# Fit the linear regression model with country fixed effects
r6 <- lm(tc_mid_dummy_ICOW ~ land_oriented_medium_continuous + autocracy_BX + milex_constant2010us_AFM + + land_CONT + cinc_MC + country_dummies, data = dat3)
# Now that I have country fixed effects, I can add these onto my table and visualize them with everything else
htmlreg(list(r1, r2, r3, r4, r5, r6),  # added in r6 which is my new regression with FE
        file = "/Users/elijahbakam/Library/CloudStorage/GoogleDrive-bakam@usc.edu/.shortcut-targets-by-id/19DfrFWKARGNjNlJ0nheNeauY-8DKul1a/412_413 shared AY23-24/Elijah Bakam/effects.on.MIDS.html", 
        caption = "Effects on Territorial Militarized Interstate Disputes", 
        caption.above = TRUE, 
        custom.header = list("Economic Orientation" = 1, "Regime Type" = 2, "Military Expenditures" = 3, "Land Borders" = 4, "Military Capabilities" = 5, "R5 + FE" = 6), 
        stars = c(0.001, 0.01, 0.05), 
        custom.note = "Variables come from the replication data of the Productive Pacifists paper published in International Studies Quarterly, F.E. = Fixed Effects",
        omit.coef = "country", # this clears out having 400 rows of countries 
        custom.gof.rows = list("Country FE" = c("No", "No", "No", "No", "No", "Yes"))) # adds a row to the bottom of the table that denotes which models, if any, used country fixed effects

# END OF HOMEWORK































































