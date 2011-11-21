/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>
#include <time.h>

#include "engine.h"
#include "matrix.h"
#include "mex.h"

void wait( int seconds )
{
    clock_t endwait = clock () + seconds * CLOCKS_PER_SEC ;
    while (clock() < endwait) {}
}

int main( int argc, char** argv )
{
    if( argc != 2 )
    {
        std::cout << "Error: Need one argument specifying a simulink model!" << std::endl;
        //std::cout << "For more information enter: " << argv[ 0 ] << " --help" << std::endl;
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
    matlabCommand = "set_param('" + modelName + "','SimulationCommand', 'start')";
    engEvalString(ep, matlabCommand.c_str() );

    matlabCommand = "status = get_param('" + modelName + "','SimulationStatus')";
    engEvalString(ep, matlabCommand.c_str() );

    mxArray *mxTimeArray;
    mxArray *resultArray = engGetVariable(ep,"status");   //returns 'running' or 'stopped'
    char * resultString = mxArrayToString( resultArray );
    std::cout << "status = " << resultString << std::endl;
    double simTime;
    do 
    {
        wait( 1 );
        matlabCommand = "time = get_param('" + modelName + "','SimulationTime')";
        engEvalString(ep, matlabCommand.c_str() );
        mxTimeArray = engGetVariable(ep,"time");
        simTime = mxGetScalar( mxTimeArray );
        std::cout << "simulation time = " << simTime << std::endl;
/*
        resultArray = engGetVariable(ep,"status");   //returns 'running' or 'stopped'
        resultString = mxArrayToString( resultArray );
        std::cout << "status = " << resultString << std::endl;
*/
//    } while ( strcmp(resultString, "running") == 0 );
    } while ( simTime != 0 );

/*
%callback for the stop simulation button
set_param('mymodel', 'SimulationCommand', 'stop') %send the stop command to it
%you can also do something after stopping the simulation
simdata = evalin('base', 'simout'); %get the simulation data exported by the block T W
myfun(simdata) %just an example, insert here your own function 
*/
    // use fgetc() to pause
    std::cout << "Hit return to continue" << std::endl;
    fgetc(stdin);
    
    // We're done! Free memory, close MATLAB engine and exit.
    std::cout << "Freeing memory, closing MATLAB engine, and exiting" << std::endl;
    mxDestroyArray( resultArray );
    mxDestroyArray( mxTimeArray );
    engClose(ep);
    
    return( EXIT_SUCCESS );
}
