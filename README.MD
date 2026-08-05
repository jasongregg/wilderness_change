#### This is a repository for Gregg Belote Linck wilderness change study project


**Scripts*

script_1.R does basic data cleaning and prep.

script_5.R is most of the analysis. It creates csv tables using terra:freq and a block-based approach
It also creates wilderness rasters and will do pixel transition analysis (pending)

script_figures.R generates all the plots and figures, using the previously generated csv outputs.


**Data**

Includes all geo-spatial data used for analysis. **Large NLCD data  are not uploaded, large file size**

**Outputs**

Includes 2025 outputs generated during original analysis.

Includes all generated .csv files and draft manuscript paper plots. **clipped wilderness rasters are not uploaded, large file size**

Includes suppl. material tables based on csv files.

Includes draft manuscript in markdown.





#####
######LAB NOTEBOOK########
#####

***July 30***
Created a quarto file to share writing and figures. 
Created some figures

**March 6**
I matched extents of ecoregion and wilderness rasters to NLCD data.

Then I stacked rasters and tried to do a combine approach using terra::crosstab

It seemed to work but the output is unweildy and includes tons of 0s for combinations without use

Looking at the crosstab, it also only included < 300 unique wilderness IDs, even though the 
input raster contains all 751. unsure why but is likely an issue

I think its worth bookmarking this approach but, given wildernesses cover such a small area relative to the NLCD rasters
it makes more sense to subset the data

**March 7**
Tried stacking rasters and using a block-based approach. Seems to finally have let me count wilderness pixels by
ecoregion and state

Unfortunately, the pixel count for the three csv files are very different. I was hoping it would identify the same 
total number of wilderness NLCD pixels at the ecoregion and state level.

Checked ecoregion and state rasters and rerun.

There seem to be gaps between NLCD data and the ecoregion - I mapped these and its mainly coastal.

What about between ecoregion and wilderness?


**maybe try the crosstab approach with tempest**

**MARCH8**
fixed my state and ecoregion rasters by assigning a new field with integers only.

Saved these rasters in "data/raster_stack" 

then, ran the block extract and came up with pixel counts for the three sets of data: wilderness,
ecoregion, and state, which are all within about 1% in terms of pixel number. The discrepancy seems to be that 
some wilderness areas extend outside of ecoregions/states? 

Surprisingly, compared with my previous method, there are far more pixels in these data than when 
I used polygons to extract. No idea why.

**To do**
Go over what exactly the block extract does, how it works. What does the non-blocked version of that code 
look like? Can tempest run it?

Why did the raster approach end up with fewer pixels for the wilderness only data: 
248421823 vs 246601846 pixels? Is it because the raster only counts pixels whose center is inside polygon,
vs intersection of polygon? I can try re-running the rasterization with touches = TRUE which 
counts any pixel that touches the polygon boundary. Does this make them more similar?

Calculate which pixels are being dropped, how many, and where they are between datasets in a methodical way

Try Crosstabs again now that you've fixed your rasters. Maybe just run it on tempest

Start re-assigning your wilderness, state, ecoregion, and NLCD names to your CSV data

**March 11**

I re-ran the lower 48 rasterization to fix the GEOID field. Then I re-ran the block extract
on this new raster to try and resolve the mismatched ID issue.

TO DO

Now try appending field information from states, ecoregions, NLCD, and wilderness 

Then you should be ready to generate plots.

The only outstanding question is whether you want to more carefully rasterize wilderness areas
with a 50% pixel coverage inclusion rule. Its probably not worth it unless you can run on tempest.

The other option is to try a crosstabs approach, but this doesn't seem like it should be a priority
even though it may be a more elegant approach.

**March 15**
I re-appended state and wilderness names to the csv files I generated.

Now just need to fix the ecoregion file by also appending.

**April 12**

Opened script and README for the first time  since March - now need to figure out what I did and whats next

To DO

1) Append eco region csv - one of the scripts should have the previous appends to do this

2) clean the script and METHODIZE it so that there is a clear trail of what you did

3) Make/remake figures after looking over what you have

4) Work on into

5) get on github and send to travis and ethan


****June 16th***

I opened scripts and csv files and reoriented myself. I also updated R, renamed scripts.

I created a clean new script, script_5.r based on the block-based pixel extraction method that encompasses the entire workflow 
that I can share with Travis. Because of this, its probably worth re-doing the analysis to make sure it works.

I pulled in code from script_3.r

****June 17***

I kept working on script 5 and appended and saved the l2ecoregion csv.

Appended csvs are now in a dedicated folder in /outputs

I continued to clean script 5. It not has been rerun from scratch yet

I created script 6 which will be figure making and analysis

Now I need to make some more figures (for l2ecoregion csv) and then get it on github.

Github: include 1) scripts 5 and 6 2) outputs folder with appended csvs and figures and 3)
a draft manuscript

raster data too large for github.






# wilderness_change
