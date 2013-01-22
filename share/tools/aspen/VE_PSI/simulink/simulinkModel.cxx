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

#include <boost/filesystem/operations.hpp>

#include <fstream>
#include <iostream>
#include <sstream>

simulinkModel::simulinkModel( std::string inputFileNameAndPath )
{
    this->ep = NULL;

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
    //std::cout << "\nReading file '" << this->modelName << "'" << std::endl;

    std::cout << "Starting MATLAB engine" << std::endl;

    // Call engOpen with a NULL string. This starts a MATLAB process on the current host
    if ( ! ( this->ep = engOpen("") ) )
    {
        std::cerr << "Error: Can't start MATLAB engine" << std::endl;
        return;
    }

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

simulinkModel::~simulinkModel()
{
#ifdef PRINT_HEADERS
    std::cout << "deleting simulinkModel\n" << std::endl;
#endif // PRINT_HEADERS

    if( this->ep != NULL )
    {
        std::cout << "Freeing memory and closing MATLAB engine." << std::endl;
        engClose( this->ep );
    }
}

int simulinkModel::DoesNotExist()
{
    if( this->ep == NULL )
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

void simulinkModel::StartSimulation()
{
    // Use SimulationCommand to start, stop, pause, continue, update 
    std::cout << "Starting simulation" << std::endl;
    this->matlabCommand = "set_param('" + this->modelName + "','SimulationCommand', 'start');";
    engEvalString( this->ep, this->matlabCommand.c_str() );
}

std::string simulinkModel::GetSimulationStatus()
{
    std::string status = GetSimulinkString( "SimulationStatus" );
    return status;
}

double simulinkModel::GetSimulationTime()
{
    double simTime = GetSimulinkParameter( "SimulationTime" );
    return simTime;
}

void simulinkModel::ReadBlockNames()
{
    this->matlabCommand = "blks = find_system('" + this->modelName + "', 'Type', 'block');";
    engEvalString( this->ep, this->matlabCommand.c_str() );

    this->numBlocks = GetResultFromMatlabCommand( "length(blks);" );
    //std::cout << "numBlocks = " << this->numBlocks << std::endl;

    for( int i=1; i<= this->numBlocks; i++ )
    {
        std::stringstream out;
        out << i;
        //block name data is stored in cells rather than directly as strings
        //thus can't access as blks(i). Instead use blks{i,1}.
        std::string blockName = GetStringFromMatlabCommand( "blks{" + out.str() + ",1};" );
        this->blockNameList.push_back( blockName );
    }
}

int simulinkModel::GetNumBlocks()
{
    if( this->blockNameList.empty() )
    {
        this->ReadBlockNames();
    }

    return this->numBlocks;
}

std::vector<std::string> simulinkModel::GetBlockNames()
{
    if( this->blockNameList.empty() )
    {
        this->ReadBlockNames();
    }

    return this->blockNameList;
}

std::string simulinkModel::GetBlockName( unsigned int blockNumber )
{
    if( this->blockNameList.empty() )
    {
        this->ReadBlockNames();
    }

    if( this->blockNameList.size() > blockNumber )
    {
        return this->blockNameList.at( blockNumber );
    }
    else
    {
        std::cerr << "ERROR: no blockName for blockNumber = " << blockNumber << std::endl;
        return "";
    }
}

std::vector<std::string> simulinkModel::GetParameterNames( unsigned int blockNumber )
{
    std::vector<std::string> parameterNameList;

    if( this->blockNameList.empty() )
    {
        this->ReadBlockNames();
    }

    if( this->blockNameList.size() > blockNumber )
    {
        std::string blockName = this->blockNameList[ blockNumber ];

        // get list of parameter names in a 1x1 struct
        this->matlabCommand = "parameters = fieldnames(get_param('" + blockName + "','dialogparameters'));";
        engEvalString( this->ep, this->matlabCommand.c_str() );

        int numParameters = GetResultFromMatlabCommand( "length(parameters);" );
        //std::cout << blockName << ", numParameters = " << numParameters << std::endl;

        for( int j=1; j<= numParameters; j++ )
        {
            std::stringstream jj;
            jj << j;

            std::string parameterName = GetStringFromMatlabCommand( "parameters{" + jj.str() + "};" );
            parameterNameList.push_back( parameterName );
 
            std::string parameterValue = GetStringFromMatlabCommand( "get_param('" + blockName + "','" + parameterName + "');" );
            //std::cout << "   " << parameterName << " = " << parameterValue << std::endl;
        }
    }
    else
    {
        std::cerr << "ERROR: no blockName for blockNumber = " << blockNumber << std::endl;
    }

    return parameterNameList;
}

void simulinkModel::SetParameter( std::string blockName, std::string parameterName, std::string newValue )
{
    this->matlabCommand = "set_param('" + blockName + "', '" + parameterName + "', '" + newValue + "');";
    engEvalString( this->ep, this->matlabCommand.c_str() );
}

std::string simulinkModel::GetParameter( std::string blockName, std::string parameterName )
{
    return GetStringFromMatlabCommand( "get_param('" + blockName + "','" + parameterName + "');" );
}

double simulinkModel::GetResultFromMatlabCommand( std::string matlabCommand )
{
    engEvalString( this->ep, matlabCommand.c_str() );
    mxArray *tempMxArray = engGetVariable( this->ep,"ans" );
    double returnValue = mxGetScalar( tempMxArray );
    mxDestroyArray( tempMxArray );
    return( returnValue );
}

double simulinkModel::GetSimulinkParameter( std::string paramString )
{
    std::string matlabCommand = "get_param('" + this->modelName + "','" + paramString + "');";
    return( GetResultFromMatlabCommand( matlabCommand ) );
}

std::string simulinkModel::GetStringFromMatlabCommand( std::string matlabCommand )
{
    // Evaluate the MATLAB command and store answer in tempMxArray:
    engEvalString( this->ep, matlabCommand.c_str() );
    mxArray *tempMxArray = engGetVariable( this->ep,"ans" );

    // Most parameters seem to be returned as text (char array). 
    // However a few are stored in double arrays, structs, or other data classes.
    // For now we will convert any non-text data into text.

    //std::cout << mxGetClassName( tempMxArray ) << std::endl;

    if( mxIsChar( tempMxArray ) )
    {
        char * resultString = mxArrayToString( tempMxArray );
        mxDestroyArray( tempMxArray );
        return( std::string(resultString) );
    }
    else if( mxIsDouble( tempMxArray ) )
    {
        int numRows = mxGetM( tempMxArray );                  
        int numCols = mxGetN( tempMxArray );
        //std::cout << "numRows = " << numRows << ", numCols = " << numCols << std::endl;

        if( numRows > 1 )
        {
            return( "TBD 2D double data" );
        }
        else    // single row vector...
        {
            double *doubleArray = mxGetPr( tempMxArray );        
            mxDestroyArray( tempMxArray );

            std::string returnString = "[ ";

            for (int i=0; i<numCols; i++)
            {
                //std::cout << doubleArray[ i ] << std::endl;
                std::stringstream tempString;
                tempString << doubleArray[ i ];
                returnString = returnString + tempString.str() + " ";
            }
            returnString = returnString + "]";
            return( returnString );
        }
    }
    else if( mxIsStruct( tempMxArray ) )
    {
        //int numRows = mxGetM( tempMxArray );                  
        //int numCols = mxGetN( tempMxArray );
        //std::cout << "numRows = " << numRows << ", numCols = " << numCols << std::endl;
        //void *Ar = mxGetData(tempMxArray);
        return( "TBD struct data" );
    }
    else if( mxIsCell( tempMxArray ) )
    {
        return( "TBD cell data" );
    }
    else
    {
        return( "TBD class data" );
    }
}

std::string simulinkModel::GetSimulinkString( std::string paramString )
{
    std::string matlabCommand = "get_param('" + this->modelName + "','" + paramString + "');";
    return( GetStringFromMatlabCommand( matlabCommand ) );
}


/*
%callback for the stop simulation button
set_param('mymodel', 'SimulationCommand', 'stop') %send the stop command to it
%you can also do something after stopping the simulation
simdata = evalin('base', 'simout'); %get the simulation data exported by the block T W
myfun(simdata) %just an example, insert here your own function 
*/

