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

#ifndef FERMENTOR_UI_H
#define FERMENTOR_UI_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIPluginBase.h>

// --- wxWidgets Includes --- //
#include <wx/image.h>

// --- C/C++ Libraries --- //
#include <string>

class FermentorUI : public ves::conductor::UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( Fermentor )

public:
    FermentorUI();
    virtual ~FermentorUI();

    virtual double GetVersion();
    //Return the version number of the module

    virtual ves::conductor::UIDialog* UI( wxWindow* parent );
    //This returns the UI dialog of the module

    virtual wxString GetConductorName();

    wxString GetName();
    //This returns the name of the module

    double agitation;
    double air_conc;
    double ini_ph;
    double nitrate_conc;
    double temperature;
    double hours;

    long cycle_ID;
    long rotation_ID;
    long xray_ID;
    long loop_ID;

    double rot_speed;
    double sim_speed;
    //HERE is the GUI variable passed to the Dialog and Packed
  
protected:
};

#endif //FERMENTOR_UI_H

