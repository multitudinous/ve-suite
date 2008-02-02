//This is the code written to calculate changes in the concentration of
//citric acid in the Indian Hills fermentor.  

// --- C/C++ Libraries
#include <iostream>
#include <fstream>

double r = 200;
//Defines the agitiation (in rpm) in the fermentor
double a = 1.25;
//Defines the concentration of air initially
double p = 6;
//Defines the initial pH in the fermentor
double n = 0.1;
//Defines the initial nitrate concentration
double k = 37;
//Defines the initial temperature in celsius
double numsteps = 240;
//Defines the number of iterations to perform

std::fstream results;
//File for statistical results

void main()
{
    //cout << "Hello?" << endl;

    double t;         //time (in hours)
    //double s;         //sugar concentration
    double c[ 8 ];    //array of equation answers
    int i;            //looping index

    //cout << "I am working" << endl;

    results.open( "values.txt", std::ios::out ); 

    //cout << "I'm starting the loop" << endl;


    for( t = 0; t < numsteps; ++t )
    {
        c[ 1 ] = -0.000036 * t * t * t + 0.0092 * t * t - 0.072 * t + 1;
        c[ 2 ] = -0.000091 * r * r + 0.035 * r - 2.56;
        c[ 3 ] = -1 * a * a + 2 * a -2;
        c[ 4 ] = -0.41 * p * p + 4.9 * p - 13;
        c[ 5 ] = -17 * n * n + 8.4 * n - 0.004;
        c[ 6 ] = -0.01 * k * k + 0.69 * k - 7.8;
        c[ 7 ] = -1;
        c[ 0 ] = 1;

        //cout << "Timestep " << t << endl;

        for( i = 1; i < 8; ++i )
        {
            c[ 0 ] = c[ 0 ] * c[ i ];
        }
        //cout << "I calculated the concentration" << endl;

        results << c[ 0 ] << std::endl;
    }

    //close the results file
    results.close();     
}
