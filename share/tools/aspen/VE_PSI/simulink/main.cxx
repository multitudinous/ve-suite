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

#include <simulinkModel.h>

#include <cstdlib>
#include <iostream>
#include <string>
#include <ctime>

void wait( int seconds )
{
    clock_t endwait = clock () + seconds * CLOCKS_PER_SEC ;
    while( clock() < endwait ) {}
}

// Test model ves_test.mdl uses Simulink Real Time Execution function available at matlabCentral-fileExchange.
// http://www.mathworks.com/matlabcentral/fileexchange/21908-simulink%C2%AE-real-time-execution

int main( int argc, char** argv )
{
    if( argc != 2 )
    {
        std::cout << "Error: Need one argument specifying a simulink model!" << std::endl;
        std::cout << "    For example: " << argv[ 0 ] << " ves_test" << std::endl;
        std::cout << "    (note that '.mdl' extension is not used)" << std::endl;
        return( EXIT_FAILURE );
    }

    // Start a new reader object using the second commandline argument as input filename...
    std::string modelName( argv[ 1 ] );
    simulinkModel* model = new simulinkModel( modelName );
    if( model->DoesNotExist() )
    {
        return( EXIT_FAILURE );
    }

    model->StartSimulation();

    // status is either 'running' or 'stopped'
    std::string status = model->GetSimulationStatus();
    std::cout << "status = " << status << std::endl;

    double simTime;
    do 
    {
        wait( 1 );

        simTime = model->GetSimulationTime();
        status = model->GetSimulationStatus();
        std::cout << "simulation time = " << simTime << ", status = " << status << std::endl;
    }
    while( status == "running" );

    // Get a list of the blocks in the system
    std::vector<std::string> blockNames = model->GetBlockNames();
    std::cout << "numBlocks = " << blockNames.size() << std::endl;
    
    for( int block=0; block< model->GetNumBlocks(); block++ )
    {
        std::cout << model->GetBlockName( block ) << std::endl;
        
        std::vector<std::string> parameterNames = model->GetParameterNames( block );
        /*
        std::cout << "parameterNames = " << &parameterNames << std::endl;
        
        int numParameters = parameterNames.size();
        if( numParameters == 0 )
        {
            continue;
        }

        for( int i=1; i<= numParameters; i++ )
        {
            std::cout << "   " << parameterNames[ i ] << std::endl;
        }
        */
    }

    // Test of setting existing parameter...
    std::string blockName = "ves_test/Gain";
    std::string parameterName = "Gain";
    std::string newValue = "2";
    model->SetParameter( blockName, parameterName, newValue );

    std::string parameterValue = model->GetParameter( blockName, parameterName );
    std::cout << "\n   new parameterValue  = " << parameterValue << std::endl;

    // use fgetc() to pause
    std::cout << "\nHit return to continue" << std::endl;
    fgetc(stdin);
   
    delete model;

    return( EXIT_SUCCESS );
}
