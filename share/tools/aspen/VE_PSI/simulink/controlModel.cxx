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

#include "engine.h"
#include "matrix.h"

//#define BUFSIZE 256

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

    // start the simulation
    std::cout << "Starting simulation" << std::endl;
    // Use SimulationCommand to start, stop, pause, continue, update 
    matlabCommand = "set_param('" + modelName + "','SimulationCommand', 'start')";
    engEvalString(ep, matlabCommand.c_str() );
/*
    char buffer[BUFSIZE+1];
    buffer[BUFSIZE] = '\0';
    engOutputBuffer(ep, buffer, BUFSIZE);
*/
    matlabCommand = "status = get_param('" + modelName + "','SimulationStatus')";
    engEvalString(ep, matlabCommand.c_str() );
    mxArray *resultArray = engGetVariable(ep,"status");   //returns 'running' or 'stopped'
    if( resultArray )
    {
        char * resultString = mxArrayToString( resultArray );
        std::cout << "status = " << resultString << std::endl;

        if( strcmp(resultString, "running") == 0 )
        {
            matlabCommand = "time = get_param('" + modelName + "','SimulationTime')";
            engEvalString(ep, matlabCommand.c_str() );
            mxArray *timeArray = engGetVariable(ep,"time");   //returns 'running' or 'stopped'
            //std::cout << "time = " << timeArray[ 0 ] << std::endl;
        }
    }

    // Echo the output from the command.  
    //std::cout << buffer << std::endl;

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
    mxDestroyArray(resultArray);
    engClose(ep);
    
    return( EXIT_SUCCESS );
}
