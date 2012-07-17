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

#include <boost/algorithm/string/trim.hpp>
#include <boost/filesystem/operations.hpp>

#include <fstream>
#include <iostream>

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

simulinkModel::simulinkModel( std::string inputFileNameAndPath )
{
    this->existFlag = 1;

    // Do some tests to make sure provided name is a simulink model filename in correct format...
    boost::filesystem::path ext = boost::filesystem::path( inputFileNameAndPath ).extension();
#ifdef PRINT_HEADERS
    std::cout << "\nextension = " << ext << std::endl;
#endif // PRINT_HEADERS
    if( ext == ".mdl" )
    {
        std::cerr << "\nWarning: Extension '.mdl' is not expected on input file '"
                  << inputFileNameAndPath << "'." << std::endl;
        std::cerr << "Extension will be stripped to allow program to continue. " << std::endl;
        inputFileNameAndPath = boost::filesystem::path( inputFileNameAndPath ).stem().string();
    }
    else if( ext != "" )
    {
        std::cerr << "\nError: Provided filename is not of correct format." << std::endl;
        std::cerr << "Should be a simulink model file without '.mdl' extension" << std::endl;
        return;
    }

    // Make sure that simulink model filename exists...
    boost::filesystem::path full_path = boost::filesystem::path( inputFileNameAndPath ).replace_extension( ".mdl" );
#ifdef PRINT_HEADERS
    std::cout << "\nTesting exist status of file " << full_path << std::endl;
#endif // PRINT_HEADERS

    if( ! boost::filesystem::exists( full_path ) )
    {
        std::cerr << "Error: input file does not exist or is not readable." << std::endl;
        return;
    }

    this->modelName = inputFileNameAndPath;
    std::cout << "\nReading file '" << this->modelName << "'" << std::endl;

    std::cout << "Starting MATLAB engine" << std::endl;

    // Call engOpen with a NULL string. This starts a MATLAB process on the current host
    if ( ! ( this->ep = engOpen("") ) )
    {
        std::cerr << "Error: Can't start MATLAB engine" << std::endl;
        return;
    }

    this->existFlag = 0;    // All good to go
}

simulinkModel::~simulinkModel()
{
#ifdef PRINT_HEADERS
    std::cerr << "deleting simulinkModel\n" << std::endl;
#endif // PRINT_HEADERS

    // Free memory, close MATLAB engine and exit.
    std::cout << "Freeing memory, closing MATLAB engine, and exiting" << std::endl;
    engClose( this->ep );
}

int simulinkModel::DoesNotExist()
{
    return this->existFlag;
}

void simulinkModel::open()
{
    // open the simulink window with your model
    std::cout << "Opening simulink model '" << this->modelName << "'" << std::endl;
    this->matlabCommand = "open_system('" + this->modelName + "')";
    engEvalString( this->ep, this->matlabCommand.c_str() );
/*
    // load the model but don't show it in a window
    matlabCommand = "load_system('" + modelName + "')";
    engEvalString(ep, matlabCommand.c_str() );
*/
}

void simulinkModel::startSimulation()
{
    // Use SimulationCommand to start, stop, pause, continue, update 
    std::cout << "Starting simulation" << std::endl;
    this->matlabCommand = "set_param('" + this->modelName + "','SimulationCommand', 'start');";
    engEvalString( this->ep, this->matlabCommand.c_str() );
}

std::string simulinkModel::GetSimulationStatus()
{
    std::string status = GetSimulinkString( this->ep, this->modelName, "SimulationStatus" );
    return status;
}

double simulinkModel::GetSimulationTime()
{
    double simTime = GetSimulinkParameter( this->ep, this->modelName, "SimulationTime" );
    return simTime;
}

void simulinkModel::ReadBlockNames()
{
    this->matlabCommand = "blks = find_system('" + this->modelName + "', 'Type', 'block');";
    engEvalString( this->ep, this->matlabCommand.c_str() );

    this->numBlocks = GetResultFromMatlabCommand( this->ep, "length(blks);" );
    std::cout << "numBlocks = " << this->numBlocks << std::endl;

    for( int i=1; i<= this->numBlocks; i++ )
    {
        std::stringstream out;
        out << i;
        //data is stored in cells rather than directly as strings
        //thus can't access as blks(i). Instead use blks{i,1}.
        std::string blockName = GetStringFromMatlabCommand( ep, "blks{" + out.str() + ",1};" );
        this->blockNameList.push_back( blockName );
    }
}

void simulinkModel::ReadParameterNames( int blockNumber )
{
    std::string blockName = this->blockNameList[ blockNumber ];

    // get list of parameter names in a 1x1 struct
    this->matlabCommand = "parameters = fieldnames(get_param('" + blockName + "','dialogparameters'));";
    engEvalString( this->ep, this->matlabCommand.c_str() );

    int numParameters = GetResultFromMatlabCommand( this->ep, "length(parameters);" );
    std::cout << blockName << ", numParameters = " << numParameters << std::endl;

    for( int j=1; j<= numParameters; j++ )
    {
        std::stringstream jj;
        jj << j;

        std::string parameterName = GetStringFromMatlabCommand( ep, "parameters{" + jj.str() + "};" );

        // Something wierd about Scope block. Parameters are not accessed in the way other blocks are.
        // Workaround: For Scope block, just list parameters without trying to get parameter values.
        if( blockName.substr(blockName.size()-5,5) == "Scope")
        {
            std::cout << "   " << parameterName << std::endl;
        }
        else
        {
            std::string parameterValue = GetStringFromMatlabCommand( this->ep, "get_param('" + blockName + "','" + parameterName + "');" );
            std::cout << "   " << parameterName << " = " << parameterValue << std::endl;
        }
    }
}

int simulinkModel::GetNumBlocks()
{
    return this->numBlocks;
}

void simulinkModel::SetParameter( std::string blockName, std::string parameterName, std::string newValue )
{
    this->matlabCommand = "set_param('" + blockName + "', '" + parameterName + "', '" + newValue + "');";
    engEvalString( this->ep, this->matlabCommand.c_str() );
}

std::string simulinkModel::GetParameter( std::string blockName, std::string parameterName )
{
    return GetStringFromMatlabCommand( this->ep, "get_param('" + blockName + "','" + parameterName + "');" );
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

