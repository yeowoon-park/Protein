clear all

cd "C:\Users\siegm\Desktop\Dropbox\논문\Seafood\seafood_aids\STATA\8th analysis"

---634(100) pmeat psea pplant
clear all

import excel protein634.xlsx, firstrow clear

destring bmiS, force replace
destring income, force replace

g lninc = ln(income)

g cos1 = cos(1/6*3.14159*t)
g sin1 = sin(1/6*3.14159*t)

aidsills wmeat wsea wplant, prices(pmeat psea pplant) expenditure(E) intercept(child_index ageS t cos1 sin1) ivexpenditure(lninc) symmetry alpha_0(100)
estimates store stage1

aidsills wmeat wsea wplant, prices(pmeat psea pplant) expenditure(E) intercept(child_index ageS bmiS t cos1 sin1) ivexpenditure(lninc) symmetry alpha_0(100)
estimates store stage2

aidsills wmeat wsea wplant, prices(pmeat psea pplant) expenditure(E) intercept(child_index ageS hcpls t cos1 sin1) ivexpenditure(lninc) symmetry alpha_0(100)
estimates store stage3

aidsills wmeat wsea wplant, prices(pmeat psea pplant) expenditure(E) intercept(child_index ageS bmiS hcpls t cos1 sin1) ivexpenditure(lninc) symmetry alpha_0(100)
estimates store stage4

test rho_vE
quietly test [wmeat]rho_vE, notest
quietly test [wsea]rho_vE, notest accumulate
test [wplant], accumulate

xml_tab stage1 stage2 stage3 stage4, save(C:\Users\siegm\Desktop\Dropbox\논문\Seafood\seafood_aids\STATA\8th analysis\result\protein634(2).xml)

---621(-30) pfish pcephal pclam pcrus
clear all

import excel sea631.xlsx, firstrow clear

destring bmiS, force replace
destring income, force replace

g lninc = ln(income)

g cos1 = cos(1/6*3.14159*t)
g sin1 = sin(1/6*3.14159*t)

g lnEsea = ln(Esea)

aidsills w2fish w2cephal w2clam w2crus, prices(pfish pcephal pclam pcrus) expenditure(Esea) intercept(child_index ageS t cos1 sin1) ivexpenditure(lninc) symmetry alpha_0(-30)
estimates store stage1

aidsills w2fish w2cephal w2clam w2crus, prices(pfish pcephal pclam pcrus) expenditure(Esea) intercept(child_index ageS bmiS t cos1 sin1) ivexpenditure(lninc) symmetry alpha_0(-30)
estimates store stage2

aidsills w2fish w2cephal w2clam w2crus, prices(pfish pcephal pclam pcrus) expenditure(Esea) intercept(child_index ageS hcpls t cos1 sin1) ivexpenditure(lninc) symmetry alpha_0(-30)
estimates store stage3

aidsills w2fish w2cephal w2clam w2crus, prices(pfish pcephal pclam pcrus) expenditure(Esea) intercept(child_index ageS bmiS hcpls t cos1 sin1) ivexpenditure(lninc) symmetry alpha_0(-30)
estimates store stage4

test rho_vEsea
return list

xml_tab stage1 stage2 stage3 stage4, save(C:\Users\siegm\Desktop\Dropbox\논문\Seafood\seafood_aids\STATA\8th analysis\sea631(3).xml)

---634(50) pbeef ppork pchicken
clear all

import excel meat634.xlsx, firstrow clear

destring bmiS, force replace
destring income, force replace

g cos1 = cos(1/6*3.14159*t)
g sin1 = sin(1/6*3.14159*t)

g lninc = ln(income)

aidsills w2beef w2pork w2chicken, prices(pbeef ppork pchicken) expenditure(Emeat) intercept(child_index ageS t cos1 sin1) ivexpenditure(lninc) symmetry alpha_0(50)
estimates store stage1

aidsills w2beef w2pork w2chicken, prices(pbeef ppork pchicken) expenditure(Emeat) intercept(child_index ageS bmiS t cos1 sin1) ivexpenditure(lninc) symmetry alpha_0(50)
estimates store stage2

aidsills w2beef w2pork w2chicken, prices(pbeef ppork pchicken) expenditure(Emeat) intercept(child_index ageS hcpls t cos1 sin1) ivexpenditure(lninc) symmetry alpha_0(50)
estimates store stage3

aidsills w2beef w2pork w2chicken, prices(pbeef ppork pchicken) expenditure(Emeat) intercept(child_index ageS bmiS hcpls t cos1 sin1) ivexpenditure(lninc) symmetry alpha_0(50)
estimates store stage4

xml_tab stage1 stage2 stage3 stage4, save(C:\Users\siegm\Desktop\Dropbox\논문\Seafood\seafood_aids\STATA\8th analysis\meat634(2).xml)





























