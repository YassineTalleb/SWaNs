app_copy <- function() {
  list(
    hero = list(
      eyebrow = "Scientific decision support for wastewater source tracking",
      title = "SWaNs",
      subtitle = paste(
        "Source localization through sequential wastewater sampling in sewer networks.",
        "Upload an adjacency matrix and node weights, validate the structure, and",
        "compare deterministic sampling strategies in a reproducible analytical workflow."
      )
    ),
    overview = list(
      title = "Overview",
      paragraphs = c(
        paste(
          "This application supports the analysis of wastewater-based source localisation",
          "from raw network data to reproducible strategy comparison."
        ),
        paste(
          "Internally, the sewer network is treated as a directed rooted tree whose root",
          "represents the outlet. A wastewater sample result at one node is positive when the true source lies",
          "in that node's upstream subtree."
        )
      ),
      workflow = c(
        "1. Provide one adjacency matrix file and one weight file.",
        "2. Validate tree structure, root, and weight coverage.",
        "3. Simulate source localisation under different strategies and numbers of samplers.",
        "4. Read the outputs through the number of sampling cycles, the number of samples, and full cycle distributions."
      )
    ),
    uploads = list(
      title = "Data Input",
      network = c(
        "The sewer network file must be provided as an adjacency matrix.",
        "Convention: a value of 1 in row A and column B means that wastewater flows from A to B.",
        "For text uploads, the app automatically detects comma, semicolon, tab, and pipe delimiters.",
        "The app validates the uploaded matrix, can offer a transpose when the flow direction appears reversed, and converts the result into an internal outlet-rooted tree.",
        "Uploaded files are copied into temporary session storage for processing and are removed automatically when replaced or when the session ends."
      ),
      weights = c(
        "The weight file must contain node identifiers in the first column.",
        "The second column should contain a non-negative weight such as population.",
        "Weights are normalised into prior source probabilities."
      )
    ),
    strategy_notes = strategy_catalog(),
    assumptions = list(
      title = "Assumptions",
      items = c(
        "Exactly one source emits the biomarker.",
        "Wastewater sample results are deterministic and error-free.",
        "A positive sample result means the source lies in the sampled upstream subtree.",
        "Weights define the prior likelihood that a node is the source."
      )
    ),
    limitations = list(
      title = "Limitations",
      items = c(
        "Hydraulic transport, dilution, travel time, and assay uncertainty are not modelled.",
        "The app assumes a rooted tree, not a general cyclic sewer graph.",
        "If several sources can occur at once, the strategy logic must be extended.",
        "Weight choice matters: population is transparent, but not always epidemiologically sufficient."
      )
    ),
    comparison = list(
      title = "Strategy Comparison",
      paragraphs = c(
        paste(
          "kGBS aims for balanced information gain by partitioning the remaining prior mass.",
          "MRP prioritises likely source nodes, while MCRP and SMCRP favour large subtrees."
        ),
        paste(
          "The main trade-off is not simplicity versus complexity, but early direct hits",
          "versus a more even reduction of the remaining search space."
        )
      )
    ),
    parallel_recommendation = list(
      title = "Parallelization Recommendation",
      paragraphs = c(
        paste(
          "The recommended range of the number of samplers is derived from marginal efficiency within the",
          "evaluated comparison results and uses kGBS whenever that strategy is included."
        ),
        paste(
          "For each evaluated number of samplers k, the app compares the expected reduction in the number of sampling cycles to the",
          "additional expected number of samples required when moving from the previous evaluated number of samplers to k."
        ),
        paste(
          "Sampler counts are recommended when their marginal efficiency reaches at least",
          "75% of the peak positive value observed in the evaluated range."
        )
      )
    ),
    figure_guides = list(
      network_overview = list(
        title = "How to read this sewer network view",
        items = c(
          "Node size shows the direct prior probability of that node.",
          "Node color shows the cumulative upstream probability contained in that node's upstream set.",
          "Darker blue-green shading means more remaining probability mass upstream of that node."
        )
      ),
      guided_network = list(
        title = "How to read this sewer network guidance view",
        items = c(
          "Node size shows the direct prior probability of that node.",
          "Possible source nodes are shown in blue-green, recommended nodes in gold, selected positive nodes in dark blue-green, and excluded nodes in red.",
          "A thick dark border marks the current focus node. Select positive results through the checklist below the recommendations."
        )
      ),
      comparison = list(
        title = "How to read these comparison plots",
        items = c(
          "Line plots show probability-weighted expected performance based on the uploaded node weights.",
          "Boxplots of the number of sampling cycles show the unweighted spread across feasible source nodes, so each feasible source contributes one run.",
          "Boxplots of the number of samples show the unweighted spread of the actually required total samples until source identification, again with one deterministic run per feasible source node.",
          "The time-resource trade-off combines the number of sampling cycles and the number of samples, while the CDF view shows how quickly each strategy resolves sources."
        )
      )
    ),
    references = list(
      title = "References",
      intro = paste(
        "This application is embedded in a broader scientific workflow.",
        "The references below document the companion manuscript, the software environment,",
        "and the contact details for scientific correspondence about SWaNs."
      ),
      manuscript = list(
        label = "Companion manuscript",
        status = "Submitted manuscript",
        title = paste(
          "Source-tracking algorithms for wastewater-based epidemiology in urban sewer networks:",
          "Design and comparative evaluation in a German metropolitan area"
        ),
        authors = paste(
          "Yassine Talleb, Lukas Pape, Tina Schmidt, Issa Nafo,",
          "Susanne Moebus, Katja Ickstadt, and Dennis Schmiege"
        ),
        note = paste(
          "Companion methodological manuscript, currently submitted, describing the",
          "graph-theoretic framework, algorithmic design, and comparative evaluation",
          "underlying SWaNs. Further bibliographic details will be added once available."
        )
      ),
      software = list(
        list(
          authors = "R Core Team",
          year = "2021",
          title = "R: A Language and Environment for Statistical Computing",
          source = "R Foundation for Statistical Computing, Vienna, Austria",
          link = "https://www.R-project.org/"
        ),
        list(
          authors = "Chang W, Cheng J, Allaire JJ, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B",
          year = "2022",
          title = "shiny: Web Application Framework for R",
          source = "R package version 1.7.3",
          link = "https://CRAN.R-project.org/package=shiny"
        ),
        list(
          authors = "Sievert C, Cheng J",
          year = "2022",
          title = "bslib: Custom 'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown'",
          source = "R package version 0.4.1",
          link = "https://CRAN.R-project.org/package=bslib"
        ),
        list(
          authors = "Wickham H, Bryan J",
          year = "2022",
          title = "readxl: Read Excel Files",
          source = "R package version 1.4.1",
          link = "https://CRAN.R-project.org/package=readxl"
        ),
        list(
          authors = "Cheng J, Sievert C, Schloerke B, Chang W, Xie Y, Allen J",
          year = "2022",
          title = "htmltools: Tools for HTML",
          source = "R package version 0.5.3",
          link = "https://CRAN.R-project.org/package=htmltools"
        )
      ),
      correspondence = list(
        title = "Scientific correspondence",
        person = "Yassine Talleb",
        role = "Corresponding contact for the SWaNs application",
        email = "yassine.talleb@tu-dortmund.de",
        note = paste(
          "Please use this contact for questions on the application,",
          "its methodological basis, and implementation details."
        )
      )
    )
  )
}
