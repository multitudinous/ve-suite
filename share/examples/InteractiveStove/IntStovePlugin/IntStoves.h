/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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

#ifndef INT_STOVES_H
#define INT_STOVES_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIPluginBase.h>

// --- wxWidgets Includes --- //
#include <wx/image.h>

// --- C/C++ Libraries --- //
#include <vector>

class IntStoves : public ves::conductor::UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( IntStoves )

public:

    IntStoves();

    virtual ~IntStoves();

    virtual double GetVersion();

    virtual int GetNumPoly();

    virtual ves::conductor::UIDialog* UI( wxWindow* parent );

    virtual wxString GetConductorName();
    virtual wxString GetName();
    virtual wxString GetDesc();

    virtual int GetNumIports();
    virtual void GetIPorts( POLY& ports );

    virtual int GetNumOports();
    virtual void GetOPorts( POLY& ports );

protected:

private:

public:
    long numbaffles;

    std::vector< double > baffle1;
    std::vector< double > baffle2;
    std::vector< double > baffle3;
    std::vector< double > baffle4;
    std::vector< double > baffle5;
    std::vector< double > baffle6;
    std::vector< double > baffle7;
  
};

#endif //INT_STOVES_H
