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
#ifndef UIDIALOG_H
#define UIDIALOG_H
/*!\file UIDialog.h
UIDialog API
*/
/*!\class UIDialog
*
*/
#include <wx/dialog.h>
#include <wx/string.h>

#include <string>

class wxWindow;

#include <ves/VEConfig.h>

namespace ves
{
namespace conductor
{
class UIPluginBase;
namespace util
{
class CORBAServiceList;
}

class VE_GUIPLUGINS_EXPORTS UIDialog : public wxDialog
{
protected:
    UIDialog()
        :
        mUIPluginBase( 0 ),
        mCORBAService( 0 )
    {
        ;
    }
public:
    ///Constructor
    UIDialog( wxWindow* parent, int id, wxString title = wxT( "UI" ) );
    ///Destructor
    virtual ~UIDialog();
    ///This function locks/unlocks every input entry
    virtual void Lock( bool l );
    ///Set the corbaservicelist for this dialog
    void SetCORBAServiceList( util::CORBAServiceList* serviceList );
    ///Provide the managing UIPluginBase pointer to this dialog
    void SetUIPluginBase( UIPluginBase* pluginBase );
protected:
    ///Convert unicode text to a std string
    ///\param data Unicode wx string
    ///\return The std::string
    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }
    
    UIPluginBase* mUIPluginBase;
    util::CORBAServiceList* mCORBAService;
    bool lock ;
};
}
}
#endif
