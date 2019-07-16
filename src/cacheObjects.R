
library("ProjectTemplate")
reload.project(list(data_loading = TRUE, cache_loading = FALSE, munging = TRUE, 
                    load_libraries = TRUE, recursive_loading = FALSE))

ProjectTemplate::cache("mbEset")

ProjectTemplate::cache("mbAllL")
ProjectTemplate::cache("mbAllW")
ProjectTemplate::cache("mbTumW")
ProjectTemplate::cache("mbTumL")
ProjectTemplate::cache("mbPatW")
ProjectTemplate::cache("mbPatL")


