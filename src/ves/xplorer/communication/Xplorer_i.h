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
#ifndef _XPLORER_I_H_
#define _XPLORER_I_H_

#include <ves/open/moduleS.h>
#include <ves/open/idl/Body_AMI_UIHandler_i.h>

#undef _REENTRANT
#include <vpr/Sync/Mutex.h>
#include <vpr/Sync/Guard.h>
#include <vpr/IO/SerializableObject.h>

#include <ves/VEConfig.h>

#include <ves/open/xml/CommandPtr.h>

#include <vector>
#include <map>
#include <iostream>

class VE_XPLORER_COMM_EXPORTS Body_VEXplorer_i
            : public virtual POA_Body::VEXplorer
{
public:
    ///Constructor
    Body_VEXplorer_i( void );

    ///Destructor
    virtual ~Body_VEXplorer_i( void );

    virtual char* GetStatusMessage( );

    virtual void SetParams( const char* param );

    virtual void SetID( ::CORBA::Long id );

    virtual ::CORBA::Long GetID( );

    virtual void SetName( const char* name );

    virtual char* GetName( );

    virtual void SetNetwork( const char* network );

    virtual void SetCommand( const char* command );

    virtual void RegisterUI( const char* UIName, ::Body::UI_ptr ui );

    virtual void UnRegisterUI( const char* UIName );

    void PreFrameUpdate( void );
    /// Frame sync variables used by osg only at this point
    float GetSetAppTime( float );
    ///Get/Set the frame number
    long GetSetFrameNumber( long );
    ///Set the cluster mode flag
    void SetClusterMode( bool clusterFlag );
    ///Determine if we are in cluster mode or not
    bool GetClusterMode( void );

    ///Get the command queue
    std::vector <ves::open::xml::CommandPtr> GetCommandQueue();

    ves::open::xml::CommandPtr bufferCommand;///< Data to hold command data

    ///Set xplorer data
    ///\param input
    void SetXplorerData( std::string input );

protected:
    ///Create command queue for transient vis
    ///Should be removed once non texture pipelines are created
    //void CreateCommandQueue( void );

    std::vector< ves::open::xml::CommandPtr > commandVectorQueue;///< command vector may be a duplicate
    std::vector< std::string > commandStringQueue;///< command queue with raw string data

    vpr::Mutex mValueLock;  ///< A mutex to protect variables accesses

    bool isCluster;///<cluster mode

    float time_since_start;///< start time
    long frameNumber;///< frame number

    ///The conductor interfaces
    std::map<std::string, Body::UI_var> uiCom;
    ///AMI handler for asynchronous calls to conductor
    Body_AMI_UIHandler_i m_xplorerAMIHandler;
};
#endif
