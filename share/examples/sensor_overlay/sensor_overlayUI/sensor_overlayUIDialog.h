/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#ifndef SENSOR_OVERLAY_UI_DIALOG_H
#define SENSOR_OVERLAY_UI_DIALOG_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIDialog.h>

#include <ves/open/xml/DataValuePairPtr.h>

namespace ves
{
namespace conductor
{
namespace util
{
    class CORBAServiceList;
}
}
}

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

namespace sensor
{
class sensor_overlayUIDialog : public ves::conductor::UIDialog
{
public:
    sensor_overlayUIDialog();
    sensor_overlayUIDialog( wxWindow* parent,
                              int id,
                              ves::conductor::util::CORBAServiceList* service );

    virtual ~sensor_overlayUIDialog();
    
    enum SENSOR_OVERLAY_IDS
    {
        
    };

    virtual bool TransferDataFromWindow();
    virtual bool TransferDataToWindow();
    virtual void Lock( bool l );

protected:

private:
    void BuildGUI();

    void SendCommandsToXplorer();
    void ClearInstructions();

    ves::conductor::util::CORBAServiceList* mServiceList;
    std::vector< ves::open::xml::DataValuePairSharedPtr > mInstructions;
    std::string mCommandName;

    DECLARE_EVENT_TABLE()
};
} //end sensor

#endif //SENSOR_OVERLAY_UI_DIALOG_H
