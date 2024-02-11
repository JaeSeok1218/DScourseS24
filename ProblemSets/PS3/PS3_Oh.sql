#Var-name
#policyID,statecode,county,eq_9site_limit,hu_site_limit,fl_site_limit,fr_site_limit,
#tiv_2011,tiv_2012,eq_site_deductible,hu_site_deductible,fl_site_deductible,fr_site_deductible,
#point_latitude,point_longitude,line,construction,point_granularity

# (a)
CREATE TABLE Finsurance(
"var1", "var2", "var3", "var4", "var5", "var6", "var7", "var8", "var9",
"var10", "var11", "var12", "var13", "var14", "var15", "var16", "var17", "var18"
);

.mode csv
.import FL_insurance_sample.csv

# (b)
SELECT * FROM Finsurance LIMIT 10;
#(c)
SELECT DISTINCT var3 FROM Finsurance
#(d)
SELECT AVG(var9 - var8) FROM Finsurance;
#(e)
SELECT var17, COUNT(*) FROM Finsurance GROUP BY var17;