library(shiny)


shinyUI(bootstrapPage(


    headerPanel("Non-parametric Tests"),


    sidebarPanel(

        radioButtons("type", strong("Test type:"),
                    list("Mann-Whitney U-test" = "mannwhitney",
                         "Wilcoxon signed-rank test" = "wilcoxon",
                         "Kruskal-Wallis test" = "kruskalwallis",
                         "Friedman test" = "friedman"
                        ),'Mann-Whitney U-test'
        ),

        br()

    ),




mainPanel(



    tabsetPanel(

        tabPanel("Main",

            p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

            p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Please make sure that your data includes the header (variable names) in the first row.</div></b>")),

            aceEditor("text", value="Class\tScore\n1\t78\n1\t70\n1\t73\n1\t86\n1\t65\n1\t74\n1\t59\n1\t78\n1\t86\n1\t56\n1\t4\n1\t66\n1\t100\n1\t53\n1\t57\n2\t42\n2\t3\n2\t51\n2\t21\n2\t45\n2\t100\n2\t39\n2\t57\n2\t32\n2\t46\n2\t26\n2\t54\n2\t28\n2\t42\n2\t30",
                mode="r", theme="cobalt"),

            br(),

            h3("Basic statistics"),
            verbatimTextOutput("textarea.out"),

            br(),

            h3("Ranks"),
            verbatimTextOutput("ranking.out"),

            br(),

            h3("Box plots with individual data points"),
            plotOutput("boxPlot", width="80%"),

            h3("Test result"),
            verbatimTextOutput("test.out"),

            br()

            ),



    tabPanel("Input Samples",

p('Note: Input values must be separated by tabs. Copy and paste from Excel/Numbers.'),

p(HTML("<b><div style='background-color:#FADDF2;border:1px solid black;'>Please make sure that your data includes the header (variable names) in the first row.</div></b>")),

br(),

p(strong("Mann-Whitney U-test (Comparing two independent conditions)")),
aceEditor("text1", value="Class\tScore\n1\t78\n1\t70\n1\t73\n1\t86\n1\t65\n1\t74\n1\t59\n1\t78\n1\t86\n1\t56\n1\t4\n1\t66\n1\t100\n1\t53\n1\t57\n2\t42\n2\t3\n2\t51\n2\t21\n2\t45\n2\t100\n2\t39\n2\t57\n2\t32\n2\t46\n2\t26\n2\t54\n2\t28\n2\t42\n2\t30", mode="r", theme="solarized_light"),


br(),
p(strong("Wilcoxon signed-rank test (Comparing two related conditions)")),
aceEditor("text2", value="First\tSecond\n78\t42\n6\t3\n73\t51\n86\t21\n45\t45\n74\t98\n59\t59\n78\t57\n86\t84\n56\t46\n4\t26\n66\t54\n100\t28\n53\t42\n57\t30", mode="r", theme="solarized_light"),


br(),
p(strong("Kruskal-Wallis test (Differences between several independent groups)")),
aceEditor("text3", value="Class\tScore\n1\t78\n1\t70\n1\t73\n1\t86\n1\t65\n1\t74\n1\t59\n1\t78\n1\t86\n1\t56\n1\t4\n1\t66\n1\t100\n1\t53\n1\t57\n2\t42\n2\t3\n2\t51\n2\t21\n2\t45\n2\t100\n2\t39\n2\t57\n2\t32\n2\t46\n2\t26\n2\t54\n2\t28\n2\t42\n2\t30\n3\t2\n3\t42\n3\t86\n3\t85\n3\t53\n3\t90\n3\t1\n3\t69\n3\t53\n3\t74\n3\t80\n3\t66\n3\t100\n3\t70\n3\t37",mode="r", theme="solarized_light"),


br(),
p(strong("Friedman test (Differences between several related groups)")),
aceEditor("text4", value="First\tSecond\tThird\n78\t42\t42\n6\t3\t6\n73\t51\t86\n86\t21\t85\n45\t45\t53\n74\t98\t90\n59\t59\t1\n78\t57\t69\n86\t84\t53\n56\t46\t74\n4\t26\t80\n66\t54\t66\n100\t28\t100\n53\t42\t70\n57\t30\t37", mode="r", theme="solarized_light"),

br()

),






        tabPanel("About",

            strong('Note'),
            p('This web application is developed with',
            a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),
            ''),

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

            strong('Recommended'),
            p('To learn more about R, I suggest this excellent and free e-book (pdf),',
            a("A Guide to Doing Statistics in Second Language Research Using R,", href="http://cw.routledge.com/textbooks/9780805861853/guide-to-R.asp", target="_blank"),
            'written by Dr. Jenifer Larson-Hall.'),

            p('Also, if you are a cool Mac user and want to use R with GUI,',
            a("MacR", href="http://www.urano-ken.com/blog/2013/02/25/installing-and-using-macr/", target="_blank"),
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
