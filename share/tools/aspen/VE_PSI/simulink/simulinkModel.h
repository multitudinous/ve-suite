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
#ifndef SIMULINKMODEL_H
#define SIMULINKMODEL_H

#include <string>
#include <vector>

#include "engine.h"
#include "matrix.h"
#include "mex.h"

class simulinkModel
{
public:
    simulinkModel( std::string inputFileNameAndPath );

    ~simulinkModel();
    
    int DoesNotExist();

    void StartSimulation();

    std::string GetSimulationStatus();

    double GetSimulationTime();

    int GetNumBlocks();

    void SetParameter( std::string blockName, std::string parameterName, std::string newValue );

    std::string GetParameter( std::string blockName, std::string parameterName );
    
    void ReadBlockNames();

    void ReadParameterNames( int blockNumber );

private:
    std::string modelName;
    Engine *ep;
    //matlabCommand(s) are strings that you could directly enter in the MATLAB Command Window
    std::string matlabCommand;
    std::vector<std::string> blockNameList;
    int numBlocks;
    int blockNumber;

    double GetResultFromMatlabCommand( std::string matlabCommand );

    double GetSimulinkParameter( std::string paramString );

    std::string GetStringFromMatlabCommand( std::string matlabCommand );

    std::string GetSimulinkString( std::string paramString );

};
#endif

