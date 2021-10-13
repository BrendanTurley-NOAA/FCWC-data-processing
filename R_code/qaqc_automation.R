### aquatrol quality control

### Recommended methods to comply with IOOS standards
# https://ioos.noaa.gov/project/qartod/

# ##flags
# Pass=1
# Not evaluated=2
# Suspect or Of High Interest=3
# Fail=4
# Missing data=9

### tests
# Group 1
# Required
# Test 1) Gap Test
# Test 2) Syntax Test
# Test 3) Location Test
# Test 4) Gross Range Test
# Test 5) Climatological Test

# Group 2
# Strongly Recommended
# Test 6) Spike Test
# Test 7) Rate of Change Test
# Test 8) Flat Line Test

# Group 3
# Suggested
# Test 9) Multi-Variate Test
# Test 10) Attenuated Signal Test
# Test 11) Neighbor Test
# Test 12) TS Curve/Space Test
# Test 13) Density Inversion Test
  
 

###--------- aquatrol quality control
qaqc_aquatroll <- function (input # input file is the output data.frame from data_extract_aquatroll function
)
{
  
}