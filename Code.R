library(tidytuesdayR)
library(tidyverse)
library(gtExtras)
library(gt)
library(reshape2)
library(RColorBrewer)
library(ggtext)
library(scales)
tuesdata <- tidytuesdayR::tt_load(2021, week = 41)

tuesdata$nurses->nurses

glimpse(nurses)

nurses%>%filter(Year==2020)%>%
  select(State,`Total Employed RN`,
         `Annual Salary Median`)->tableaa
         
data.frame(tableaa)%>%drop_na()->tableaa

tableaa
colnames(tableaa)<-c("State","Total Employed Registered Nurses",
                   "Median Annual Salary")
                   
tableaa$`Total Employed Registered Nurses`<-(tableaa$`Total Employed Registered Nurses`)/1000
tableaa$`Median Annual Salary`<-(tableaa$`Median Annual Salary`)/1000

nurses%>%filter(Year==2019)%>%
  select(State,`Total Employed RN`,
         `Annual Salary Median`)->table1a
data.frame(table1a)%>%drop_na()->table1a
colnames(table1a)<-c("State","Total Employed Registered Nurses",
                    "Median Annual Salary")
table1a$`Total Employed Registered Nurses`<-(table1a$`Total Employed Registered Nurses`)/1000
table1a$`Median Annual Salary`<-(table1a$`Median Annual Salary`)/1000

tableaa%>%
  left_join(table1a,by="State")->table3a
data.frame(table3a)->table3a
table3a

glimpse(table3a)

colnames(table3a)<-c("State","2020 TOTAL EMPLOYED, REGISTERED NURSES","2020 MEDIAN ANNUAL SALARY",
                     "2019 TOTAL EMPLOYED, REGISTERED NURSES","2019 MEDIAN ANNUAL SALARY")
table3a[c(1,4,2,5,3)]->table3a

nurses%>%drop_na()%>%filter(Year>=2019)%>%
  distinct(State,Year,.keep_all = TRUE)%>%
  select(State,Year,`Annual Salary Median`)%>%
  mutate(`Annual Salary Median`=`Annual Salary Median`/1000)%>%
  arrange(Year,State)%>%
  group_by(State)%>%
  mutate(spark=list(`Annual Salary Median`))%>%
  select(State, spark) %>%
  distinct()->t4
table3a%>%left_join(t4,by="State")->table41a

source_tag <- "Data: <a href='https://data.world/zendoll27/registered-nursing-labor-stats-1998-2020'>Data World</a> via TidyTuesday| Design and Analysis: @annapurani93"


table41a%>%gt()%>%
  gt_sparkline(spark,
               type = "sparkline",
               range_colors = c("red","blue"))%>%
  tab_spanner(
    label = "TOTAL EMPLOYED NURSES",
    columns = c(`2019 TOTAL EMPLOYED, REGISTERED NURSES`,`2020 TOTAL EMPLOYED, REGISTERED NURSES`)
  )%>%
  tab_spanner(
    label = "MEDIAN ANNUAL SALARY",
    columns = c(`2019 MEDIAN ANNUAL SALARY`,`2020 MEDIAN ANNUAL SALARY`,spark)
  )%>%
  cols_label(
    `2019 TOTAL EMPLOYED, REGISTERED NURSES` = "2019",
    `2019 MEDIAN ANNUAL SALARY` = "2019",
    `2020 TOTAL EMPLOYED, REGISTERED NURSES`="2020",
    `2020 MEDIAN ANNUAL SALARY`="2020",
    spark="TREND"
  )%>%
  gt_color_box(columns = `2019 TOTAL EMPLOYED, REGISTERED NURSES`, domain = 0:310,
               palette = "RColorBrewer::RdBu")%>%
  gt_color_box(columns = `2020 TOTAL EMPLOYED, REGISTERED NURSES`, domain = 0:310,
               palette = "RColorBrewer::RdBu")%>%
  gt_color_rows(columns = `2019 MEDIAN ANNUAL SALARY`,type = "continuous", domain = 30:120,palette = "RColorBrewer::RdBu")%>%
  gt_color_rows(columns = `2020 MEDIAN ANNUAL SALARY`,type = "continuous", domain = 30:120,palette = "RColorBrewer::RdBu")%>%
  tab_header(
    title = md("**WHICH US STATES PAID THEIR NURSES MORE DURING THE PANDEMIC?**"),
    subtitle = "The United States is among the worst-affected country due to the coronavirus pandemic. The country has the highest number of confirmed COVID cases, and the frontline workers including doctors, nurses, and other healthcare specialists are instrumental in keeping the country and its people afloat. However, the median annual salary of the nurses in most of the US States saw little to no increase in 2020 vis-à-vis 2019. In fact, in States such as Alabama, Delaware, District of Columbia, Hawaii and Rhode Island, the annual median salary fell in 2020 when compared to 2019’s figures. 
On the other hand, in States such as California and Washington, the nurses saw a 7% increase in their annual median salaries. Here’s a summary"
  )%>%
  tab_options(
    table.background.color = "black"
  )%>%
  cols_width(everything()~px(150))%>%
  cols_align(
    align = "center",
    columns = everything())%>%
  tab_style(
    style = cell_text(align="center"),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    ))%>%
  tab_style(style = cell_text(weight="bold", transform = "uppercase"),
            locations = cells_column_labels(everything())
  )%>%
  opt_table_lines("all")%>%
  opt_table_outline()%>%
  opt_row_striping()%>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        color = "#989898",
        weight = px(1),
        style="dashed"
      ),
      cell_borders(
        sides = c("left", "right"),
        color = "#989898",
        weight = px(1),
        style="dashed"
      )),
    locations = cells_column_labels(
      columns = everything()
    )
  )%>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        color = "#989898",
        weight = px(0.2),
        style="dotted"
      ),
      cell_borders(
        sides = c("left", "right"),
        color = "#989898",
        weight = px(1),
        style="dashed"
      )),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  )%>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        color = "#989898",
        weight = px(0.2),
        style="dotted"
      ),
      cell_borders(
        sides = c("left", "right"),
        color = "#989898",
        weight = px(1),
        style="dashed"
      )),
    locations = cells_column_spanners()
  )%>%
  tab_options(
    table.align = "center",
    column_labels.background.color = "black",
    table_body.hlines.color = "#989898",
    table_body.border.top.color = "#989898",
    heading.border.bottom.color = "#989898",
  )%>%
  tab_style(
    style = list(cell_text(color = "black", weight="bold"),
                 cell_fill(color = "white")),
    locations = cells_column_spanners()
  )%>%
  tab_source_note(md(html(source_tag)))%>%
  tab_style(
    style = list(
      cell_text(
        align = "right",
        color = "white"
      )
    ),
    locations = cells_source_notes()
  )%>%
  tab_footnote(
    footnote = "Total employed, registered nurses, in thousands",
    locations = cells_column_spanners("TOTAL EMPLOYED NURSES")
  )%>%
  tab_footnote(
    footnote = "Median annual salary in thousand dollars",
    locations = cells_column_spanners("MEDIAN ANNUAL SALARY")
  )->output

gtsave(output,"output.png")









