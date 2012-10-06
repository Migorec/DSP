set terminal wxt
plot  "curve0.csv" using 1:2 with lines linewidth 5.0 title "Source signal", "curve1.csv" using 1:2 with lines linewidth 2.0 title "Restored signal"
