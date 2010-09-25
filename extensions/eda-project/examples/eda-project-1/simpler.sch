v 20100214 2
C 40000 40000 0 0 0 title-B.sym
C 48700 43400 1 0 0 gnd-1.sym
C 52300 44500 1 0 0 gnd-1.sym
C 47900 47600 1 0 0 vcc-1.sym
C 44100 45700 1 0 0 resistor-1.sym
{
T 44400 46100 5 10 0 0 0 0 1
device=RESISTOR
T 44300 46000 5 10 1 1 0 0 1
refdes=RIN
T 44100 45700 5 10 1 1 0 0 1
value=10
}
C 47200 47100 1 270 0 resistor-1.sym
{
T 47600 46800 5 10 0 0 270 0 1
device=RESISTOR
T 47500 46900 5 10 1 1 270 0 1
refdes=R1
T 47200 47100 5 10 1 1 0 0 1
value=28K
}
C 47200 45200 1 270 0 resistor-1.sym
{
T 47600 44900 5 10 0 0 270 0 1
device=RESISTOR
T 47500 45000 5 10 1 1 270 0 1
refdes=R2
T 47200 45200 5 10 1 1 0 0 1
value=2K
}
C 49000 47600 1 270 0 resistor-1.sym
{
T 49400 47300 5 10 0 0 270 0 1
device=RESISTOR
T 49300 47400 5 10 1 1 270 0 1
refdes=RC
T 49000 47600 5 10 1 1 0 0 1
value=3.3K
}
C 48700 45000 1 270 0 resistor-1.sym
{
T 49100 44700 5 10 0 0 270 0 1
device=RESISTOR
T 49000 44800 5 10 1 1 270 0 1
refdes=RE
T 48700 45000 5 10 1 1 0 0 1
value=100
}
C 52300 46100 1 270 0 resistor-1.sym
{
T 52700 45800 5 10 0 0 270 0 1
device=RESISTOR
T 52600 45900 5 10 1 1 270 0 1
refdes=RL
T 52300 46100 5 10 1 1 0 0 1
value=100K
}
C 50500 46200 1 0 0 capacitor-1.sym
{
T 50700 46900 5 10 0 0 0 0 1
device=CAPACITOR
T 50700 46700 5 10 1 1 0 0 1
refdes=COUT
T 50700 47100 5 10 0 0 0 0 1
symversion=0.1
T 50500 46200 5 10 1 1 0 0 1
value=2.2uF
}
C 49300 45100 1 270 0 capacitor-1.sym
{
T 50000 44900 5 10 0 0 270 0 1
device=CAPACITOR
T 49800 44900 5 10 1 1 270 0 1
refdes=CE
T 50200 44900 5 10 0 0 270 0 1
symversion=0.1
T 49300 45100 5 10 1 1 0 0 1
value=1pF
}
C 45700 45600 1 0 0 capacitor-1.sym
{
T 45900 46300 5 10 0 0 0 0 1
device=CAPACITOR
T 45900 46100 5 10 1 1 0 0 1
refdes=CIN
T 45900 46500 5 10 0 0 0 0 1
symversion=0.1
T 45700 45600 5 10 1 1 0 0 1
value=2.2uF
}
C 48500 45300 1 0 0 npn-3.sym
{
T 49400 45800 5 10 0 0 0 0 1
device=NPN_TRANSISTOR
T 49400 45800 5 10 1 1 0 0 1
refdes=Q1
T 48500 45300 5 10 0 0 0 0 1
model-name=2N3904
T 48500 45300 5 10 0 0 0 0 1
value=2N3904
}
N 43300 45800 44100 45800 4
{
T 43500 45900 5 10 1 1 0 0 1
netname=Vin
}
N 45000 45800 45700 45800 4
N 49100 46300 49100 46700 4
N 48800 45300 49500 45300 4
N 48800 45300 48800 45000 4
N 47300 47600 49100 47600 4
N 47300 47100 47300 47600 4
N 47300 45200 47300 46200 4
N 46600 45800 48500 45800 4
{
T 47700 45900 5 10 1 1 0 0 1
netname=Vbase
}
N 48800 43700 48800 44100 4
N 47300 43700 49500 43700 4
N 49500 43700 49500 44200 4
N 49100 46400 50500 46400 4
N 51400 46400 52400 46400 4
{
T 51900 46600 5 10 1 1 0 0 1
netname=Vout
}
N 52400 46400 52400 46100 4
N 52400 45200 52400 44800 4
N 49500 45300 49500 45100 4
N 47300 43700 47300 44300 4
C 43000 44600 1 0 0 vac-1.sym
{
T 43700 45250 5 10 1 1 0 0 1
refdes=VINPUT
T 43700 45450 5 10 0 0 0 0 1
device=vac
T 43700 45650 5 10 0 0 0 0 1
footprint=none
T 43700 45050 5 10 1 1 0 0 1
value=dc 1.6V ac 10MV SIN(0 1MV 1KHZ)
}
C 43200 44300 1 0 0 gnd-1.sym
C 53900 45000 1 0 0 gnd-1.sym
C 53800 46500 1 0 0 vcc-1.sym
C 53700 45300 1 0 0 vdc-1.sym
{
T 54400 45950 5 10 1 1 0 0 1
refdes=VCC
T 54400 46150 5 10 0 0 0 0 1
device=VOLTAGE_SOURCE
T 54400 46350 5 10 0 0 0 0 1
footprint=none
T 54400 45750 5 10 1 1 0 0 1
value=DC 15V
}
C 40800 49300 1 0 0 spice-model-1.sym
{
T 40900 50000 5 10 0 1 0 0 1
device=model
T 42100 49600 5 10 1 1 0 0 1
model-name=2N3904
T 41300 49400 5 10 1 1 0 0 1
file=./models/2N3904.mod
T 40800 49300 5 10 1 0 0 0 1
refdes=A1
}
