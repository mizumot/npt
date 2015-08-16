library(shiny)
library(shinyAce)



shinyUI(bootstrapPage(


    headerPanel("Non-parametric Tests"),


########## loading message #######################################

tags$head(tags$style(type="text/css", "
#loadmessage {
position: fixed;
top: 0px;
left: 0px;
width: 100%;
padding: 10px 0px 10px 0px;
text-align: center;
font-weight: bold;
font-size: 100%;
color: #000000;
background-color: #CCFF66;
z-index: 105;
}
")),

conditionalPanel(condition="$('html').hasClass('shiny-busy')",
tags$div("Loading...",id="loadmessage")),

###################################################################



    mainPanel(
        tabsetPanel(position = "left", 


# Mann-Whitney U-test (Comparing two independent conditions)

        tabPanel("Mann-Whitney U-test",

                h2("Mann-Whitney U-test"),

                h4("Comparing two independent conditions"),

                p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

                p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row.</div></b>")),

                aceEditor("text1", value="Class\tScore\n1\t78\n1\t70\n1\t73\n1\t86\n1\t65\n1\t74\n1\t59\n1\t78\n1\t86\n1\t56\n1\t4\n1\t66\n1\t100\n1\t53\n1\t57\n2\t42\n2\t3\n2\t51\n2\t21\n2\t45\n2\t100\n2\t39\n2\t57\n2\t32\n2\t46\n2\t26\n2\t54\n2\t28\n2\t42\n2\t30", mode="r", theme="cobalt"),

                br(),

                h3("Basic statistics"),
                verbatimTextOutput("MWU.bs.out"),

                br(),

                h3("Ranks"),
                verbatimTextOutput("MWU.ranking.out"),

                br(),

                h3("Box plots with individual data points"),
                plotOutput("MWU.boxPlot", width="80%"),

                h3("Test result"),
                verbatimTextOutput("MWU.test.out"),

                br(),
                br(),

                strong('R session info'),
                verbatimTextOutput("MWU.info.out")

        ),






# Wilcoxon signed-rank test (Comparing two related conditions)

        tabPanel("Wilcoxon signed-rank test",

                h2("Wilcoxon signed-rank test"),

                h4("Comparing two related conditions"),

                p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

                p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row.</div></b>")),

                aceEditor("text2", value="First\tSecond\n78\t42\n6\t3\n73\t51\n86\t21\n45\t45\n74\t98\n59\t59\n78\t57\n86\t84\n56\t46\n4\t26\n66\t54\n100\t28\n53\t42\n57\t30", mode="r", theme="cobalt"),

                br(),

                h3("Basic statistics"),
                verbatimTextOutput("WSR.bs.out"),

                br(),

                h3("Ranks"),
                verbatimTextOutput("WSR.ranking.out"),

                br(),

                h3("Box plots with individual data points"),
                plotOutput("WSR.boxPlot", width="80%"),

                h3("Test result"),
                verbatimTextOutput("WSR.test.out"),

                br(),
                br(),

                strong('R session info'),
                verbatimTextOutput("WSR.info.out")

        ),






# Kruskal-Wallis test (Differences between several independent groups)

        tabPanel("Kruskal-Wallis test",

                h2("Kruskal-Wallis test"),

                h4("Differences between several independent groups"),

                p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

                p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row.</div></b>")),

                aceEditor("text3", value="Class\tScore\n1\t78\n1\t70\n1\t73\n1\t86\n1\t65\n1\t74\n1\t59\n1\t78\n1\t86\n1\t56\n1\t4\n1\t66\n1\t100\n1\t53\n1\t57\n2\t42\n2\t3\n2\t51\n2\t21\n2\t45\n2\t100\n2\t39\n2\t57\n2\t32\n2\t46\n2\t26\n2\t54\n2\t28\n2\t42\n2\t30\n3\t2\n3\t42\n3\t86\n3\t85\n3\t53\n3\t90\n3\t1\n3\t69\n3\t53\n3\t74\n3\t80\n3\t66\n3\t100\n3\t70\n3\t37",mode="r", theme="cobalt"),

                br(),

                h3("Basic statistics"),
                verbatimTextOutput("KW.bs.out"),

                br(),

                h3("Ranks"),
                verbatimTextOutput("KW.ranking.out"),

                br(),

                h3("Box plots with individual data points"),
                plotOutput("KW.boxPlot", width="80%"),

                h3("Test result"),
                verbatimTextOutput("KW.test.out"),

                br(),
                br(),

                strong('R session info'),
                verbatimTextOutput("KW.info.out")

        ),






# Friedman test (Differences between several related groups)

        tabPanel("Friedman test",

                h2("Friedman test"),

                h4("Differences between several related groups"),

                p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

                p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Your data needs to have the header (variable names) in the first row.</div></b>")),

                aceEditor("text4", value="First\tSecond\tThird\n78\t42\t42\n6\t3\t6\n73\t51\t86\n86\t21\t85\n45\t45\t53\n74\t98\t90\n59\t59\t1\n78\t57\t69\n86\t84\t53\n56\t46\t74\n4\t26\t80\n66\t54\t66\n100\t28\t100\n53\t42\t70\n57\t30\t37", mode="r", theme="cobalt"),

                br(),

                h3("Basic statistics"),
                verbatimTextOutput("Friedman.bs.out"),

                br(),

                h3("Ranks"),
                verbatimTextOutput("Friedman.ranking.out"),

                br(),

                h3("Box plots with individual data points"),
                plotOutput("Friedman.boxPlot", width="80%"),

                h3("Test result"),
                verbatimTextOutput("Friedman.test.out"),

                br(),
                br(),

                strong('R session info'),
                verbatimTextOutput("Friedman.info.out")

        ),




# About

        tabPanel("About",

            strong('Note'),
            p('This web application is developed with',
            a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
            ''),

            br(),

            strong('List of Packages Used'), br(),
            code('library(shiny)'),br(),
            code('library(shinyAce)'),br(),
            code('library(psych)'),br(),
            code('library(beeswarm)'),br(),

            br(),

            strong('Code'),
            p('Source code for this application is based on',
            a('"The handbook of Research in Foreign Language Learning and Teaching" (Takeuchi & Mizumoto, 2012).', href='http://mizumot.com/handbook/', target="_blank")),

            p('The code for this web application is available at',
            a('GitHub.', href='https://github.com/mizumot/npt', target="_blank")),

            p('If you want to run this code on your computer (in a local R session), run the code below:',
            br(),
            code('library(shiny)'),br(),
            code('runGitHub("npt","mizumot")')
            ),

            br(),

            strong('Citation in Publications'),
                p('Mizumoto, A. (2015). Langtest (Version 1.0) [Web application]. Retrieved from http://langtest.jp'),

            br(),

            strong('Article'),
                p('Mizumoto, A., & Plonsky, L. (2015).', a("R as a lingua franca: Advantages of using R for quantitative research in applied linguistics.", href='http://applij.oxfordjournals.org/content/early/2015/06/24/applin.amv025.abstract', target="_blank"), em('Applied Linguistics,'), 'Advance online publication. doi:10.1093/applin/amv025'),

            br(),

            strong('Recommended'),
            p('To learn more about R, I suggest this excellent and free e-book (pdf),',
            a("A Guide to Doing Statistics in Second Language Research Using R,", href="http://cw.routledge.com/textbooks/9780805861853/guide-to-R.asp", target="_blank"),
            'written by Dr. Jenifer Larson-Hall.'),

            p('Also, if you are a cool Mac user and want to use R with GUI,',
            a("MacR", href="https://sites.google.com/site/casualmacr/home", target="_blank"),
            'is defenitely the way to go!'),

            br(),

            strong('Author'),
            p(a("Atsushi MIZUMOTO,", href="http://mizumot.com", target="_blank"),' Ph.D.',br(),
            'Associate Professor of Applied Linguistics',br(),
            'Faculty of Foreign Language Studies /',br(),
            'Graduate School of Foreign Language Education and Research,',br(),
            'Kansai University, Osaka, Japan'),

            br(),

            a(img(src="http://i.creativecommons.org/p/mark/1.0/80x15.png"), target="_blank", href="http://creativecommons.org/publicdomain/mark/1.0/"),

            p(br())

        )
    )
    )
))