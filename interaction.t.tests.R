# run a t-test within each level of a data frame column
# e.g., for each country, test whether the roi is greater for landing page "a" vs. "b"
interaction.t.tests <- function(data, iv, group_var, dv, group1 = "a", group2 = "b", p_cutoff = 0.01)
{
  for (level in levels(data[, iv]))
  {
    tmp <- dat[dat[iv] == level, ]
    res <- t.test(tmp[tmp[group_var] == group1, dv],
                  tmp[tmp[group_var] == group2, dv])
    
    if (res$p.value < p_cutoff) {
      dif <- as.numeric(res$estimate[1] - res$estimate[2])
      winner <- ifelse(dif > 0, group1, group2)
      sprintf(" *** %s: '%s' had %.2f greater %s, p = %.2f",
              level, winner, abs(dif), dv, res$p.value) %>% print
    } else {
      sprintf("%s: no difference in %s, p = %.2f",
              level, dv, res$p.value) %>% print
    }
  }
}