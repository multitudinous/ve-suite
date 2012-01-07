/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#include <cstdlib>
#include <iostream>
#include <string>
#include <iostream>
#include <sstream>
#include <ctime>

#include "engine.h"
#include "matrix.h"
#include "mex.h"

void wait( int seconds )
{
    clock_t endwait = clock () + seconds * CLOCKS_PER_SEC ;
    while( clock() < endwait ) {}
}

double GetResultFromMatlabCommand( Engine *ep, std::string matlabCommand )
{
    engEvalString(ep, matlabCommand.c_str() );
    mxArray *tempMxArray = engGetVariable(ep,"ans");
    double returnValue = mxGetScalar( tempMxArray );
    mxDestroyArray( tempMxArray );
    return( returnValue );
}

double GetSimulinkParameter( Engine *ep, std::string modelName, std::string paramString )
{
    std::string matlabCommand = "get_param('" + modelName + "','" + paramString + "');";
    return( GetResultFromMatlabCommand( ep, matlabCommand ) );
}

std::string GetStringFromMatlabCommand( Engine *ep, std::string matlabCommand )
{
    engEvalString(ep, matlabCommand.c_str() );
    mxArray *tempMxArray = engGetVariable(ep,"ans");
    char * resultString = mxArrayToString( tempMxArray );
    mxDestroyArray( tempMxArray );
    return( std::string(resultString) );
}

std::string GetSimulinkString( Engine *ep, std::string modelName, std::string paramString )
{
    std::string matlabCommand = "get_param('" + modelName + "','" + paramString + "');";
    return( GetStringFromMatlabCommand( ep, matlabCommand ) );
}

int main( int argc, char** argv )
{
    if( argc != 2 )
    {
        std::cout << "Error: Need one argument specifying a simulink model!" << std::endl;
        std::cout << "    For example: " << argv[ 0 ] << " ves_test" << std::endl;
        std::cout << "    (note that mdl extension is not used)" << std::endl;
        return( EXIT_FAILURE );
    }

    std::cout << "Starting MATLAB engine" << std::endl;

    // Call engOpen with a NULL string. This starts a MATLAB process on the current host
    Engine *ep;
    if (!(ep = engOpen("")))
    {
        std::cerr << "Error: Can't start MATLAB engine" << std::endl;
        return( EXIT_FAILURE );
    }

    std::string modelName( argv[ 1 ] );
    std::cout << "Opening simulink model '" << modelName << "'" << std::endl;

    //matlabCommand(s) are strings that you could directly enter in the MATLAB Command Window
    std::string matlabCommand;

    // open the simulink window with your model
    matlabCommand = "open_system('" + modelName + "')";
    engEvalString(ep, matlabCommand.c_str() );

/*
    // load the model but don't show it in a window
    matlabCommand = "load_system('" + modelName + "')";
    engEvalString(ep, matlabCommand.c_str() );
*/

    // Use SimulationCommand to start, stop, pause, continue, update 
    std::cout << "Starting simulation" << std::endl;
    matlabCommand = "set_param('" + modelName + "','SimulationCommand', 'start');";
    engEvalString(ep, matlabCommand.c_str() );

    //i status is either 'running' or 'stopped'
    std::string status = GetSimulinkString( ep, modelName, "SimulationStatus" );
    std::cout << "status = " << status << std::endl;

    double simTime;
    do 
    {
        wait( 1 );

        simTime = GetSimulinkParameter( ep, modelName, "SimulationTime" );
        status = GetSimulinkString( ep, modelName, "SimulationStatus" );
        std::cout << "simulation time = " << simTime << ", status = " << status << std::endl;
    }
    while( status == "running" );
    //while( simTime != 0 );

    // Get a list of the blocks in the system
    matlabCommand = "blks = find_system('" + modelName + "', 'Type', 'block');";
    engEvalString(ep, matlabCommand.c_str() );

    int numBlocks = GetResultFromMatlabCommand( ep, "length(blks);" );
    std::cout << "numBlocks = " << numBlocks << std::endl;
    for( int i=1; i<= numBlocks; i++ )
    {
        std::stringstream out;
        out << i;
        //data is stored in cells rather than directly as strings
        //thus can't access as blks(i). Instead use blks{i,1}.
        std::string blockName = GetStringFromMatlabCommand( ep, "blks{" + out.str() + ",1};" );

        // get list of parameter names in a 1x1 struct
        matlabCommand = "parameters = fieldnames(get_param('" + blockName + "','dialogparameters'));";
        engEvalString(ep, matlabCommand.c_str() );

        int numParameters = GetResultFromMatlabCommand( ep, "length(parameters);" );
        std::cout << blockName << ", numParameters = " << numParameters << std::endl;

        for( int j=1; j<= numParameters; j++ )
        {
            std::stringstream jj;
            jj << j;

            std::string parameterName = GetStringFromMatlabCommand( ep, "parameters{" + jj.str() + "};" );
            std::cout << "   " << parameterName << std::endl;
            //std::string parameterValue = GetStringFromMatlabCommand( ep, "get_param('" + blockName + "','" + parameterName + "');" );
            //std::cout << "   " << parameterName << " = " << parameterValue << std::endl;
        }
    }

/*
    //listblks = get_param(blks, 'BlockType') 
    mxArray * blocks[ numrows ];
    blocks[0] = engGetVariable(ep,"blks");
    std::cout << "blocks[0] = " << blocks[0] << std::endl;

    if ( ! mxIsChar( blocks[0] ) || mxGetNumberOfDimensions( blocks[0] ) > 2)
    {
        std::cout << "expecting char matrix" << std::endl;
    }

    mxChar *pi;
    //mxChar *po;
    pi = mxGetChars( blocks[0] );
    std::cout << "pi[0] = " << pi[0] << std::endl;

    mwSize m, n;
    m = mxGetM( blocks[0] );
    n = mxGetN( blocks[0] );
    //mxArray * blocks2[];
    //blocks2[0] = mxCreateNumericMatrix (m, n, mxCHAR_CLASS, mxREAL);
    
    //char * blocksString = mxArrayToString( blocks );
    //std::cout << "blocksString = " << blocksString << std::endl;

    char *str[100];
    int nrhs = 3;
    for (int i=0; i<nrhs; i++)
    {
        // Check input to be sure it is of type char.
        if( !mxIsChar( blocks[i] ) )
        {
            std::cout << "expecting char matrix" << std::endl;
        }
        // Copy the string data from prhs and place it into str.
        str[i] = mxArrayToString( blocks[i] );
        std::cout << "blocksString = " << str[i] << std::endl;
    }

*/


/*
%callback for the stop simulation button
set_param('mymodel', 'SimulationCommand', 'stop') %send the stop command to it
%you can also do something after stopping the simulation
simdata = evalin('base', 'simout'); %get the simulation data exported by the block T W
myfun(simdata) %just an example, insert here your own function 
*/
    // use fgetc() to pause
    std::cout << "\nHit return to continue" << std::endl;
    fgetc(stdin);
    
    // We're done! Free memory, close MATLAB engine and exit.
    std::cout << "Freeing memory, closing MATLAB engine, and exiting" << std::endl;
    engClose(ep);
    
    return( EXIT_SUCCESS );
}
