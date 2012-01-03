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
#ifndef VE_I_H_
#define VE_I_H_

/*!\file VE_i.h
VE_i API
*/
/*!\class ves::xplorer::VE_i
*
*/
#include <ves/open/moduleS.h>
#include <ves/VEConfig.h>
//do this to remove compile warning on linux platforms
#undef _REENTRANT
#include <vpr/Sync/Mutex.h>

#include <string>
#include <vector>

///Boost includes
#include <boost/concept_check.hpp>

namespace ves
{
namespace xplorer
{
namespace network
{
class VE_XPLORER_NETWORK_EXPORTS VE_i : public virtual POA_Body::UI
{
public:
    //Constructor
    VE_i( Body::Executive_ptr exec, std::string name );

    //Destructor
    virtual ~VE_i( void );

    std::string UIName_;
    std::string GetNetworkString( void );
    std::string GetStatusString( void );
    bool GetNetworkFlag( void );
    void GetNetworkFromCE( void );
    void SetNetworkString( std::string tempString );
    std::string QueryCE( const std::string& query );

protected:
    Body::Executive_var executive_;
    std::vector< std::string > networkStringBuffer;
    std::vector< std::string > statusStringBuffer;
    vpr::Mutex stringBufferLock;  /**< A mutex to protect variables accesses */
    vpr::Mutex statusBufferLock;  /**< A mutex to protect variables accesses */
    // Following moved up into public for xplorer-driven network loading. -RPT
    //void SetNetworkString( std::string tempString ); 

    virtual void UpdateNetwork(
        const char * network
    );

    virtual void UpdateModuleUI(
        CORBA::Long module_id,
        const char * msg
    );

    virtual void UpdateModuleResult(
        CORBA::Long module_id,
        const char * msg
    );

    virtual void UpdateLinkContent(
        CORBA::Long id,
        const char * msg
    );

    virtual void Raise(
        const char * notification
    );

    virtual
    void SetXplorerData(
        const char * xplorerData
    )
    {
        boost::ignore_unused_variable_warning( xplorerData );
    }

    virtual
    void SetCommand(
                    const char * openXMLCommand);
};
}
}
}
#endif
