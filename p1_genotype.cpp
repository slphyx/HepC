//saralamba@gmail.com

#include <Rcpp.h>
#include <math.h> 
using namespace Rcpp;

double popfn(double time, double K, double P0, double r){
  return(K*P0*exp(r*(time-1998))/(K+P0*(exp(r*(time-1998))-1)));
}

// [[Rcpp::export]]
List PanHepC(double time, List state, List parms){
  //Change NumericVector state to List state for collect NumericVector in state
  //get the value of each parameter from parms
  double K = parms["K"];
  double P0 = parms["P0"];
  
  double new_start = parms["new_start"];
  double standard_start = parms["standard_start"];
  double cover = parms["cover"];
  
  double r = parms["r"];
  double FI = parms["FI"];
  
  double f0f1 = parms["f0f1"];
  double f1f2 = parms["f1f2"];
  double f2f3 = parms["f2f3"];
  double f3c1 = parms["f3c1"];
  double c1c2 = parms["c1c2"];
  double c2c3 = parms["c2c3"];
  double c3c4 = parms["c3c4"];
  double c1bA = parms["c1bA"];
  double c1bB = parms["c1bB"];
  double c1bC = parms["c1bC"];
  double c1bD = parms["c1bD"];
  
  double c2bA = parms["c2bA"];
  double c2bB = parms["c2bB"];
  double c2bC = parms["c2bC"];
  double c2bD = parms["c2bD"];
  
  double c3bD = parms["c3bD"];
  double c4bD = parms["c4bD"];
  double deathc1 = parms["deathc1"];
  double deathc2 = parms["deathc2"];
  double deathc3 = parms["deathc3"];
  double deathc4 = parms["deathc4"];
  double deathbA = parms["deathbA"];
  double deathbB = parms["deathbB"];
  
  double deathbC = parms["deathbC"];
  double deathbD = parms["deathbD"];
  double deathtrn = parms["deathtrn"];
  double tranc4 = parms["tranc4"];
  double tranbA = parms["tranbA"];
  double tranbB = parms["tranbB"];

  double natdeath = parms["natdeath"];
          
  SEXP stddist = parms["std_dist"];
  NumericVector std_dist(stddist);

  
  double beta = parms["beta"];
  
  NumericVector std_cureF0 = parms["std_cureF0"];
  NumericVector std_cureF1 = parms["std_cureF1"];
  NumericVector std_cureF2 = parms["std_cureF2"];
  NumericVector std_cureF3 = parms["std_cureF3"];
  NumericVector std_cureC1 = parms["std_cureC1"];
  //double std_cureC2 = parms["std_cureC2"];
  
  NumericVector new_cureF0 = parms["new_cureF0"];
  NumericVector new_cureF1 = parms["new_cureF1"];
  NumericVector new_cureF2 = parms["new_cureF2"];
  NumericVector new_cureF3 = parms["new_cureF3"];
  NumericVector new_cureC1 = parms["new_cureC1"];
  NumericVector new_cureC2 = parms["new_cureC2"];
  NumericVector new_cureC3 = parms["new_cureC3"];
  NumericVector new_cureC4 = parms["new_cureC4"];
  
  //get the state values  
  //These are all vectors with genotype 1 to 6
  NumericVector S = NumericVector::create(state["S_g1"],state["S_g2"],state["S_g3"],state["S_g4"],state["S_g5"],state["S_g6"]);
  
  NumericVector C1 = NumericVector::create(state["C1_g1"],state["C1_g2"],state["C1_g3"],state["C1_g4"],state["C1_g5"],state["C1_g6"]); 
  NumericVector C2 = NumericVector::create(state["C2_g1"],state["C2_g2"],state["C2_g3"],state["C2_g4"],state["C2_g5"],state["C2_g6"]);
  NumericVector C3 = NumericVector::create(state["C3_g1"],state["C3_g2"],state["C3_g3"],state["C3_g4"],state["C3_g5"],state["C3_g6"]);
  NumericVector C4 = NumericVector::create(state["C4_g1"],state["C4_g2"],state["C4_g3"],state["C4_g4"],state["C4_g5"],state["C4_g6"]);
  NumericVector F0 = NumericVector::create(state["F0_g1"],state["F0_g2"],state["F0_g3"],state["F0_g4"],state["F0_g5"],state["F0_g6"]);
  NumericVector F1 = NumericVector::create(state["F1_g1"],state["F1_g2"],state["F1_g3"],state["F1_g4"],state["F1_g5"],state["F1_g6"]);
  NumericVector F2 = NumericVector::create(state["F2_g1"],state["F2_g2"],state["F2_g3"],state["F2_g4"],state["F2_g5"],state["F2_g6"]);
  NumericVector F3 = NumericVector::create(state["F3_g1"],state["F3_g2"],state["F3_g3"],state["F3_g4"],state["F3_g5"],state["F3_g6"]);;
  
  NumericVector HCC_A = NumericVector::create(state["HCC_A_g1"],state["HCC_A_g2"],state["HCC_A_g3"],state["HCC_A_g4"],state["HCC_A_g5"],state["HCC_A_g6"]);
  NumericVector HCC_B = NumericVector::create(state["HCC_B_g1"],state["HCC_B_g2"],state["HCC_B_g3"],state["HCC_B_g4"],state["HCC_B_g5"],state["HCC_B_g6"]);
  NumericVector HCC_C = NumericVector::create(state["HCC_C_g1"],state["HCC_C_g2"],state["HCC_C_g3"],state["HCC_C_g4"],state["HCC_C_g5"],state["HCC_C_g6"]);
  NumericVector HCC_D = NumericVector::create(state["HCC_D_g1"],state["HCC_D_g2"],state["HCC_D_g3"],state["HCC_D_g4"],state["HCC_D_g5"],state["HCC_D_g6"]);
  
  NumericVector C1std_cured = NumericVector::create(state["C1std_cured_g1"],state["C1std_cured_g2"],state["C1std_cured_g3"],state["C1std_cured_g4"],state["C1std_cured_g5"],state["C1std_cured_g6"]);
  NumericVector C1new_cured = NumericVector::create(state["C1new_cured_g1"],state["C1new_cured_g2"],state["C1new_cured_g3"],state["C1new_cured_g4"],state["C1new_cured_g5"],state["C1new_cured_g6"]);
  NumericVector C2new_cured = NumericVector::create(state["C2new_cured_g1"],state["C2new_cured_g2"],state["C2new_cured_g3"],state["C2new_cured_g4"],state["C2new_cured_g5"],state["C2new_cured_g6"]);
  NumericVector C3new_cured = NumericVector::create(state["C3new_cured_g1"],state["C3new_cured_g2"],state["C3new_cured_g3"],state["C3new_cured_g4"],state["C3new_cured_g5"],state["C3new_cured_g6"]);
  NumericVector C4new_cured = NumericVector::create(state["C4new_cured_g1"],state["C4new_cured_g2"],state["C4new_cured_g3"],state["C4new_cured_g4"],state["C4new_cured_g5"],state["C4new_cured_g6"]);
  
  NumericVector death = NumericVector::create(state["death_g1"],state["death_g2"],state["death_g3"],state["death_g4"],state["death_g5"],state["death_g6"]);
  NumericVector deathHCC = NumericVector::create(state["deathHCC_g1"],state["deathHCC_g2"],state["deathHCC_g3"],state["deathHCC_g4"],state["deathHCC_g5"],state["deathHCC_g6"]);
  NumericVector deathC14 = NumericVector::create(state["deathC14_g1"],state["deathC14_g2"],state["deathC14_g3"],state["deathC14_g4"],state["deathC14_g5"],state["deathC14_g6"]);
  
  //These are all vectors with genotype 1 to 6   
  double pop;
  pop = popfn(time, K, P0, r);
    
  double treated1;
  if((new_start>time) && (time >= standard_start)) 
    treated1 = (1000+(time-standard_start)*200);
  else
    treated1 = 0;
  
  double treated2;
  if(new_start > time)
    treated2 = 0;
  else
    treated2 = cover*(3000+(time-new_start)*200);
  
  
  NumericVector std_tr = treated1*std_dist;
 
  NumericVector new_dist(8);  
  //F0
  //###
  if(sum(C4) > 0)
    new_dist[0] = 0.05;
  else if(sum(C3) > 0 )
    new_dist[0] = 0.05;
  else if(sum(C2) > 0)
    new_dist[0] = 0.05;
  else if(sum(C1) > 0)
    new_dist[0] = 0.05;
  else if(sum(F3) > 0 )
    new_dist[0] = 0.05;
  else if(sum(F2) > 0)
    new_dist[0] = 0.05;
  else if(sum(F1) > 0)
    new_dist[0] = 0.05;
  else if(sum(F0) > 0)
    new_dist[0] = 1;
  else
    new_dist[0] = 0;
  
  //F1
  if(sum(C4)> 0)
    new_dist[1] = 0.05;
  else if(sum(C3) > 0)
    new_dist[1] = 0.05;
  else if(sum(C2) > 0)
    new_dist[1] = 0.05;
  else if(sum(C1) > 0)
    new_dist[1] = 0.05;
  else if(sum(F3) > 0)
    new_dist[1] = 0.05;
  else if(sum(F2) > 0)
    new_dist[1] = 0.05;
  else if(sum(F1) > 0)
    new_dist[1] = 0.9+0.05;
  else 
    new_dist[1] = 0;
  
  //F2
  if(sum(C4) > 0)
    new_dist[2] = 0.15;
  else if(sum(C3) > 0)
    new_dist[2] = 0.15;
  else if(sum(C2) > 0)
    new_dist[2] = 0.15;
  else if(sum(C1) > 0)
    new_dist[2] = 0.15;
  else if(sum(F3) > 0)
    new_dist[2] = 0.15;
  else if(sum(F2) > 0)
    new_dist[2] = 0.9;
  else
    new_dist[2] = 0;
  
  //F3
  if(sum(C4) > 0)
    new_dist[3] = 0.15;
  else if(sum(C3) > 0)
    new_dist[3] = 0.15;
  else if(sum(C2) > 0)
    new_dist[3] = 0.15;
  else if(sum(C1) > 0)
    new_dist[3] = 0.15;
  else if(sum(F3) > 0)
    new_dist[3] = 0.15*5;
  else 
    new_dist[3] = 0;

  //C1
  if(sum(C4) > 0)
    new_dist[4] = 0.15;
  else if(sum(C3) > 0)
    new_dist[4] = 0.15;
  else if(sum(C2) > 0)
    new_dist[4] = 0.15;
  else if(sum(C1) > 0)
    new_dist[4] = 0.15+0.15+0.15+0.15;
  else
    new_dist[4] = 0;
  
  //C2
  if(sum(C4) > 0)
    new_dist[5] = 0.15;
  else if(sum(C3) > 0)
    new_dist[5] = 0.15;
  else if(sum(C2) > 0)
    new_dist[5] = 0.15+0.15+0.15;
  else 
    new_dist[5] = 0;
  

  //C3
  if(sum(C4) > 0)
    new_dist[6] = 0.15;
  else if(sum(C3) > 0)
    new_dist[6] = 0.15+0.15;
  else
    new_dist[6] = 0;
  
  //C4
  if(sum(C4) > 0)
    new_dist[7] = 0.15;
  else
    new_dist[7] = 0;

  NumericVector new_tr = new_dist*treated2;
//###
  
  double flowin = FI;
  NumericVector infect = (F0+F1+F2+F3+C1+C2+C3+C4+HCC_A+HCC_B+HCC_C+HCC_D);
//###sum of "infect" from all 6 genotypes
  NumericVector lam0 = beta*infect/pop;
//### S and all compartments are array of 6 genotypes
  NumericVector dS = flowin*pop-lam0*S-natdeath*S;
    
  NumericVector dF0 = -f0f1*F0+lam0*S-(std_tr[0]*std_cureF0)-(new_tr[0]*new_cureF0)-natdeath*F0;
    
  NumericVector dF1 =  f0f1*F0 -f1f2*F1-std_tr[1]*std_cureF1-new_tr[1]*new_cureF1-natdeath*F1;
    
  NumericVector dF2 = f1f2*F1-f2f3*F2-std_tr[2]*std_cureF2-new_tr[2]*new_cureF2-natdeath*F2;
    
  NumericVector dF3 = f2f3*F2-f3c1*F3-std_tr[3]*std_cureF3-new_tr[3]*new_cureF3-natdeath*F3;
    
  NumericVector dC1 = f3c1*F3-deathc1*C1-c1c2*C1-std_tr[4]*std_cureC1-new_tr[4]*new_cureC1-(c1bA+c1bB+c1bC+c1bD)*C1-natdeath*C1;
    
  NumericVector dC2 = c1c2*C1-deathc2*C2-c2c3*C2-new_tr[5]*new_cureC2-(c2bA+c2bB+c2bC+c2bD)*C2-natdeath*C2;
    
  NumericVector dC3 = c2c3*C2-deathc3*C3-c3c4*C3-new_tr[6]*new_cureC3-c3bD*C3-natdeath*C3;
    
    
  NumericVector dC4 = c3c4*C3-new_tr[7]*new_cureC4-deathc4*C4-c4bD*C4-natdeath*C4;
    
  
  NumericVector dHCC_A = c1bA*(C1+C1std_cured+C1new_cured)+c2bA*(C2+C2new_cured)-deathbA*HCC_A-tranbA*HCC_A-natdeath*HCC_A;
    
  NumericVector dHCC_B = c1bB*(C1+C1std_cured+C1new_cured)+c2bB*(C2+C2new_cured)-deathbB*HCC_B-tranbB*HCC_B-natdeath*HCC_B;
    
  NumericVector dHCC_C = c1bC*(C1+C1std_cured+C1new_cured)+c2bC*(C2+C2new_cured)-deathbC*HCC_C-natdeath*HCC_C;
    
  NumericVector dHCC_D = c1bD*(C1+C1std_cured+C1new_cured)+c2bD*(C2+C2new_cured)-deathbD*HCC_D+c3bD*(C3+C3new_cured)+c4bD*(C4+C4new_cured)-natdeath*HCC_D;
    
  NumericVector ddeath =  deathc1*C1+deathc2*C2+deathc3*C3+deathc4*C4+deathbA*HCC_A+deathbB*HCC_B+deathbC*HCC_C+deathbD*HCC_D;

    
  NumericVector ddeathHCC = deathbA*HCC_A+deathbB*HCC_B+deathbC*HCC_C+deathbD*HCC_D;
    
  NumericVector ddeathC14 = deathc1*C1+deathc2*C2+deathc3*C3+deathc4*C4;
  
  
  NumericVector dC1std_cured = std_tr[4]*std_cureC1-natdeath*(C1std_cured)-(c1bA+c1bB+c1bC+c1bD)*C1std_cured;
    
  NumericVector dC1new_cured = new_tr[4]*new_cureC1-natdeath*(C1new_cured)-(c1bA+c1bB+c1bC+c1bD)*C1new_cured;
    
  NumericVector dC2new_cured = new_tr[5]*new_cureC2-natdeath*(C1new_cured)-(c2bA+c2bB+c2bC+c2bD)*C2new_cured;
    
  NumericVector dC3new_cured = new_tr[6]*new_cureC3-natdeath*(C3new_cured)-c3bD*C3new_cured;
    
  NumericVector dC4new_cured = new_tr[7]*new_cureC4-natdeath*(C4new_cured)-c4bD*C4new_cured;
    
  NumericVector new_tranLiv=tranc4*((C4-new_tr[7])+new_tr[7]*(1-new_cureC4))+tranbA*HCC_A+tranbB*HCC_B;
    
  NumericVector new_HCV_HCC=c1bA*C1+c2bA*C2+c1bB*C1+c2bB*C2+c1bC*C1+c2bC*C2+c1bD*C1+c2bD*C2;
  
  
  //###still calculate for each genotype, must sum again to get the total
  NumericVector new_death = deathc1*C1+deathc2*C2+deathc3*C3+deathc4*C4+deathbA*HCC_A+deathbB*HCC_B+deathbC*HCC_C+deathbD*HCC_D;
  
  NumericVector HCC = HCC_A+HCC_B+HCC_C+HCC_D;
  
  NumericVector incHCC = c1bA*(C1+C1std_cured+C1new_cured)+c2bA*(C2+C2new_cured)+c1bB*(C1+C1std_cured+C1new_cured)+c2bB*(C2+C2new_cured)+c1bC*(C1+C1std_cured+C1new_cured)+c2bC*(C2+C2new_cured)+c1bD*(C1+C1std_cured+C1new_cured)+c2bD*(C2+C2new_cured)+c3bD*(C3+C3new_cured)+c4bD*(C4+C4new_cured);
  
  
//###still calculate for each genotype, must sum again to get the total
  
  //use sum() to F0,F1,F2,F3
  NumericVector propF(4);  
  propF[0] = sum(F0)/(sum(F0)+sum(F1)+sum(F2)+sum(F3));
  propF[1] = sum(F1)/(sum(F0)+sum(F1)+sum(F2)+sum(F3));
  propF[2] = sum(F2)/(sum(F0)+sum(F1)+sum(F2)+sum(F3));
  propF[3] = sum(F3)/(sum(F0)+sum(F1)+sum(F2)+sum(F3));

//###still calculate for each genotype, must sum again to get the total
      
  NumericVector prev = 100*(infect/pop);
  
  NumericVector total_HCC = HCC_A+HCC_B+HCC_C+HCC_D;
  NumericVector total_HCV = F0+F1+F2+F3+C1+C2+C3+C4;
    

  NumericVector compartments(126);
  // compartments["S"] = dS;
  // compartments["F0"] = dF0;
  // compartments["F1"] = dF1;
  // compartments["F2"] = dF2;
  // compartments["F3"] = dF3;
  // compartments["C1"] = dC1;
  // compartments["C2"] = dC2;
  // compartments["C3"] = dC3;
  // compartments["C4"] = dC4;
  // compartments["HCC_A"] = dHCC_A;
  // compartments["HCC_B"] = dHCC_B;
  // compartments["HCC_C"] = dHCC_C;
  // compartments["HCC_D"] = dHCC_D;
  // compartments["death"] = ddeath;
  // compartments["deathHCC"] = ddeathHCC;
  // compartments["deathC14"] = ddeathC14;
  // compartments["C1std_cured"] = dC1std_cured;
  // compartments["C1new_cured"] = dC1new_cured;
  // compartments["C2new_cured"] = dC2new_cured;
  // compartments["C3new_cured"] = dC3new_cured;
  // compartments["C4new_cured"] = dC4new_cured;
  compartments[0] = dS[0];
  compartments[1] = dS[1];
  compartments[2] = dS[2];
  compartments[3] = dS[3];
  compartments[4] = dS[4];
  compartments[5] = dS[5];
  
  compartments[6] = dF0[0];
  compartments[7] = dF0[1];
  compartments[8] = dF0[2];
  compartments[9] = dF0[3];
  compartments[10] = dF0[4];
  compartments[11] = dF0[5];
  
  compartments[12] = dF1[0];
  compartments[13] = dF1[1];
  compartments[14] = dF1[2];
  compartments[15] = dF1[3];
  compartments[16] = dF1[4];
  compartments[17] = dF1[5];
  
  compartments[18] = dF2[0];
  compartments[19] = dF2[1];
  compartments[20] = dF2[2];
  compartments[21] = dF2[3];
  compartments[22] = dF2[4];
  compartments[23] = dF2[5];
  
  compartments[24] = dF3[0];
  compartments[25] = dF3[1];
  compartments[26] = dF3[2];
  compartments[27] = dF3[3];
  compartments[28] = dF3[4];
  compartments[29] = dF3[5];
  
  compartments[30] = dC1[0];
  compartments[31] = dC1[1];
  compartments[32] = dC1[2];
  compartments[33] = dC1[3];
  compartments[34] = dC1[4];
  compartments[35] = dC1[5];
  
  compartments[36] = dC2[0];
  compartments[37] = dC2[1];
  compartments[38] = dC2[2];
  compartments[39] = dC2[3];
  compartments[40] = dC2[4];
  compartments[41] = dC2[5];
  
  compartments[42] = dC3[0];
  compartments[43] = dC3[1];
  compartments[44] = dC3[2];
  compartments[45] = dC3[3];
  compartments[46] = dC3[4];
  compartments[47] = dC3[5];
  
  compartments[48] = dC4[0];
  compartments[49] = dC4[1];
  compartments[50] = dC4[2];
  compartments[51] = dC4[3];
  compartments[52] = dC4[4];
  compartments[53] = dC4[5];
  
  compartments[54] = dHCC_A[0];
  compartments[55] = dHCC_A[1];
  compartments[56] = dHCC_A[2];
  compartments[57] = dHCC_A[3];
  compartments[58] = dHCC_A[4];
  compartments[59] = dHCC_A[5];
  
  compartments[60] = dHCC_B[0];
  compartments[61] = dHCC_B[1];
  compartments[62] = dHCC_B[2];
  compartments[63] = dHCC_B[3];
  compartments[64] = dHCC_B[4];
  compartments[65] = dHCC_B[5];
  
  compartments[66] = dHCC_C[0];
  compartments[67] = dHCC_C[1];
  compartments[68] = dHCC_C[2];
  compartments[69] = dHCC_C[3];
  compartments[70] = dHCC_C[4];
  compartments[71] = dHCC_C[5];
  
  compartments[72] = dHCC_D[0];
  compartments[73] = dHCC_D[1];
  compartments[74] = dHCC_D[2];
  compartments[75] = dHCC_D[3];
  compartments[76] = dHCC_D[4];
  compartments[77] = dHCC_D[5];
  
  compartments[78] = ddeath[0];
  compartments[79] = ddeath[1];
  compartments[80] = ddeath[2];
  compartments[81] = ddeath[3];
  compartments[82] = ddeath[4];
  compartments[83] = ddeath[5];
  
  compartments[84] = ddeathHCC[0];
  compartments[85] = ddeathHCC[1];
  compartments[86] = ddeathHCC[2];
  compartments[87] = ddeathHCC[3];
  compartments[88] = ddeathHCC[4];
  compartments[89] = ddeathHCC[5];
  
  compartments[90] = ddeathC14[0];
  compartments[91] = ddeathC14[1];
  compartments[92] = ddeathC14[2];
  compartments[93] = ddeathC14[3];
  compartments[94] = ddeathC14[4];
  compartments[95] = ddeathC14[5];
  
  compartments[96] = dC1std_cured[0];
  compartments[97] = dC1std_cured[1];
  compartments[98] = dC1std_cured[2];
  compartments[99] = dC1std_cured[3];
  compartments[100] = dC1std_cured[4];
  compartments[101] = dC1std_cured[5];
  
  compartments[102] = dC1new_cured[0];
  compartments[103] = dC1new_cured[1];
  compartments[104] = dC1new_cured[2];
  compartments[105] = dC1new_cured[3];
  compartments[106] = dC1new_cured[4];
  compartments[107] = dC1new_cured[5];
  
  compartments[108] = dC2new_cured[0];
  compartments[109] = dC2new_cured[1];
  compartments[110] = dC2new_cured[2];
  compartments[111] = dC2new_cured[3];
  compartments[112] = dC2new_cured[4];
  compartments[113] = dC2new_cured[5];
  
  compartments[114] = dC3new_cured[0];
  compartments[115] = dC3new_cured[1];
  compartments[116] = dC3new_cured[2];
  compartments[117] = dC3new_cured[3];
  compartments[118] = dC3new_cured[4];
  compartments[119] = dC3new_cured[5];
  
  compartments[120] = dC4new_cured[0];
  compartments[121] = dC4new_cured[1];
  compartments[122] = dC4new_cured[2];
  compartments[123] = dC4new_cured[3];
  compartments[124] = dC4new_cured[4];
  compartments[125] = dC4new_cured[5];
  
  List outlist(5);
  outlist[0] = compartments;
  outlist[1] = prev;
  outlist[2] = incHCC;
  outlist[3] = pop;
  outlist[4] = infect;
  
  return outlist;
  
  
}



/*** R

*/
