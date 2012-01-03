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
#include "ModelNode.h"

#include <iostream>

namespace iaf
{
namespace scheduler
{
////////////////////////////////////////////////////////////////////////////////
ModelNode::ModelNode()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ModelNode::~ModelNode()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ModelNode::Preprocess()
{
    std::cout << "Setup the inputs for this model " << m_modelName << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void ModelNode::RunModel()
{
    std::cout << "Run this model " << m_modelName << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void ModelNode::Postprocess()
{
    std::cout << "Package the results for this model " << m_modelName << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void ModelNode::SetModelName( std::string const& name )
{
    m_modelName = name;
}
////////////////////////////////////////////////////////////////////////////////
std::string ModelNode::GetResults()
{
    std::string tempString("results");
    return tempString;
}
////////////////////////////////////////////////////////////////////////////////
}
}
