/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

#ifndef CPSI_H
#define CPSI_H

// --- ATL Includes --- //
#include <atlstr.h>

// --- Powersim Includes --- //
#include "PsSimEng.h"

#ifdef CPSI_LIBRARY
#define CPSI_EXPORTS    __declspec( dllexport )
#else
#define CPSI_EXPORTS    __declspec( dllimport )
#endif //CPSI_LIBRARY

namespace cpsi
{

class CPSI_EXPORTS Project
{
public:
    ///Constructor
    Project();

    ///Destructor
    ~Project();

    ///Open a Powersim project file
    ///\param fileName
    void Open(
        const ATL::CString& fileName,
        const ATL::CString& key,
        const ATL::CString& password );

protected:

private:
    ///
    //ATL::CComPtr< ISimulationEngine > m_simulationEngine;

    ///
    //ATL::CComPtr< ISimulationProject > m_simulationProject;

    ///
    //ATL::CComPtr< ISimulation > m_simulation;

};

/*
class CPSI_EXPORTS Variable
{
public:
    ///
    Variable();

    ///
    ~Variable();

protected:

private:


};
*/

} //end cpsi

#endif //CPSI_H
