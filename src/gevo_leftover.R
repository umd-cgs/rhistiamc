distribute_gevo_leftover <-
function(dat, pop, ref_year = 2023,
                                     suffix = " (leftover distributed)", verbose = TRUE) {
    iso3    <- "^[A-Z]{3}$"
    fuels   <- c("Battery-Electric", "Plug-in Hybrid", "Fuel-Cell-Electric", "Internal Combustion")
    classes <- c("Bus", "Light-Duty Vehicle|Car", "Light-Duty Vehicle|Two-Wheeler",
                 "Light-Duty Vehicle|Van", "Truck")
    keyv <- c("model", "scenario", "variable", "iso", "year")
    
    pop <- pop %>% filter(variable == "Population", grepl(iso3, iso)) %>%
        distinct(iso, year, .keep_all = TRUE) %>% select(iso, year, pop = value)
    keep  <- pop %>% filter(year == ref_year, !is.na(pop), pop > 0) %>% pull(iso)
    years <- sort(unique(dat$year))
    pop_full <- pop %>% filter(iso %in% keep) %>%
        complete(iso, year = years) %>% filter(year %in% years) %>%
        arrange(iso, year) %>% group_by(iso) %>% fill(pop, .direction = "downup") %>% ungroup() %>%
        filter(!is.na(pop))
    
    d <- dat %>%
        filter(grepl("^(Sales|Stocks)\\|Transportation\\|", variable)) %>%
        mutate(metric = sub("\\|.*$", "", variable),
               rest   = sub("^(Sales|Stocks)\\|Transportation\\|", "", variable),
               last   = sub("^.*\\|", "", rest),
               fuel   = ifelse(last %in% fuels, last, NA_character_),
               cls    = ifelse(is.na(fuel), rest, sub("\\|[^|]+$", "", rest))) %>%
        select(-rest, -last)
    dc <- d %>% filter(cls %in% classes)
    
    world <- dc %>% filter(iso == "World") %>%
        select(model, scenario, metric, cls, fuel, variable, unit, year, world = value)
    ctry  <- dc %>% filter(grepl(iso3, iso))
    sumc  <- ctry %>% group_by(model, scenario, variable, year) %>%
        summarise(sumc = sum(value, na.rm = TRUE), .groups = "drop")
    left  <- world %>% left_join(sumc, by = c("model", "scenario", "variable", "year")) %>%
        mutate(sumc = ifelse(is.na(sumc), 0, sumc), raw_left = world - sumc, left = pmax(raw_left, 0))
    n_clip <- sum(left$raw_left < -1e-9, na.rm = TRUE)
    
    holders <- ctry %>% filter(is.na(fuel)) %>% distinct(model, scenario, metric, cls, year, iso)
    cy  <- left %>% distinct(model, scenario, metric, cls, year)
    rec <- suppressWarnings(inner_join(cy, pop_full, by = "year")) %>%
        anti_join(holders, by = c("model", "scenario", "metric", "cls", "year", "iso")) %>%
        group_by(model, scenario, metric, cls, year) %>% mutate(w = pop / sum(pop)) %>% ungroup() %>%
        select(-pop)
    
    alloc <- suppressWarnings(inner_join(left, rec, by = c("model", "scenario", "metric", "cls", "year"))) %>%
        transmute(iso, variable, unit, year, value = left * w, model, scenario, metric, cls, fuel) %>%
        anti_join(dat, by = keyv)
    
    comb <- bind_rows(ctry, alloc)
    ldv_keys <- alloc %>% distinct(model, scenario, metric, iso, year)
    ldv <- comb %>% filter(is.na(fuel), cls %in% classes[2:4]) %>%
        semi_join(ldv_keys, by = c("model", "scenario", "metric", "iso", "year")) %>%
        group_by(model, scenario, metric, iso, year) %>%
        summarise(value = sum(value, na.rm = TRUE), unit = first(unit), .groups = "drop") %>%
        mutate(variable = paste0(metric, "|Transportation|Light-Duty Vehicle"))
    
    keys <- alloc %>% distinct(model, scenario, metric, cls, iso, year)
    kk   <- c("model", "scenario", "metric", "cls", "iso", "year")
    bev  <- comb %>% filter(fuel %in% "Battery-Electric") %>% select(all_of(kk), bev = value)
    tot  <- comb %>% filter(is.na(fuel)) %>% select(all_of(kk), total = value)
    shares <- keys %>% inner_join(bev, by = kk) %>% inner_join(tot, by = kk) %>%
        filter(total > 0) %>%
        transmute(iso, year, model, scenario, value = 100 * bev / total, unit = "%",
                  variable = paste0(ifelse(metric == "Sales", "Sales Share", "Stock Share"),
                                    "|Transportation|", cls, "|Battery-Electric")) %>%
        anti_join(dat, by = keyv)
    
    cols <- c("iso", "variable", "unit", "year", "value", "model", "scenario")
    out <- dat %>% select(all_of(cols)) %>%
        anti_join(ldv, by = keyv) %>%
        bind_rows(alloc %>% select(all_of(cols)), ldv %>% select(all_of(cols)), shares %>% select(all_of(cols))) %>%
        mutate(model = paste0(model, suffix))
    
    if (verbose) {
        cat("weight countries:", length(keep), "| allocated rows:", nrow(alloc), "| LDV rows:", nrow(ldv),
            "| share rows:", nrow(shares), "| clipped (leftover < 0):", n_clip, "\n")
        chk <- out %>% filter(grepl("^(Sales|Stocks)\\|", variable), grepl(iso3, iso)) %>%
            group_by(model, scenario, variable, year) %>% summarise(s = sum(value, na.rm = TRUE), .groups = "drop") %>%
            inner_join(out %>% filter(iso == "World") %>%
                           select(model, scenario, variable, year, w = value),
                       by = c("model", "scenario", "variable", "year")) %>%
            mutate(rel = abs(s - w) / pmax(abs(w), 1e-9))
        cat("check: cells where countries != World:", sum(chk$rel > 1e-6), "of", nrow(chk), "\n")
        cat("check: negative values in allocated rows:", any(alloc$value < 0, na.rm = TRUE), "\n")
    }
    out
}
