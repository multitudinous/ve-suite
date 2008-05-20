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

#ifndef CFD_QUAT_CAM_HANDLER_H
#define CFD_QUAT_CAM_HANDLER_H

// --- VE-Suite Includes --- //
#include <ves/open/xml/CommandPtr.h>

#include <ves/xplorer/GlobalBase.h>

#include <ves/xplorer/event/EventHandler.h>

#include <ves/xplorer/scenegraph/DCS.h>

// --- vrJuggler Includes --- //
#include <gmtl/Vec.h>

#include <vpr/Util/Timer.h>
#include <vpr/Util/Singleton.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

// --- C/C++ Libraries --- //
#include <vector>
#include <map>

namespace ves
{
namespace xplorer
{

class cfdQuatCam;

namespace scenegraph
{
class DCS;
}

/*!\file cfdQuatCamHandler.h
 * cfdQuatCamHandler API
 */
/*!\class ves::xplorer::cfdQuatCamHandler
 *
 */
class VE_XPLORER_EXPORTS cfdQuatCamHandler : public GlobalBase
{
private:
    ///Constructor
    cfdQuatCamHandler();

    ///Destructor
    ~cfdQuatCamHandler();

    ///Do not know what this is
    ///\param DeviceHandler
    vprSingletonHeader( cfdQuatCamHandler );

public:
    ///In future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand();

    ///Process the command that was set by conductor
    virtual void ProcessCommand();

    ///Set the DCS
    ///\param newDCS The new DCS
    void SetDCS( ves::xplorer::scenegraph::DCS* newDCS );

    ///This function is for quatecam handler only
    ///This should be removed once the new animation code is in place
    ///\param masterNode
    void SetMasterNode( bool masterNode );

    ///Clear out all the saved quaternions
    void ClearQuaternionData();

    ///
    ///\param
    void LoadData( ves::xplorer::scenegraph::DCS* worldDCS );

    ///
    ///\param
    void WriteToFile( std::string );

    ///
    ///\param
    void LoadFromFile( std::string );

    ///
    ///\param worldDCS
    void Relocate( ves::xplorer::scenegraph::DCS* worldDCS );

    ///
    void RemoveViewPt();

    ///
    ///\param flyindex
    ///\param ptindex
    void RemoveFlythroughPt( unsigned int flyindex, unsigned int ptindex );

    ///
    ///\param flyindex
    ///\param ptindex
    void AddViewPtToFlyThrough( unsigned int flyindex, unsigned int ptindex );

    ///
    ///\param flyindex
    ///\param beforept
    ///\param ptindex
    void InsertViewPtInFlyThrough(
        unsigned int flyindex, unsigned int beforept, unsigned int ptindex );

    ///
    ///\param flyindex
    void DeleteEntireFlythrough( unsigned int flyindex );

    ///
    void AddNewFlythrough();

    ///
    void TurnOffMovement();

    ///
    ///\param
    ///\param
    ///\return
    double getLinearDistance( gmtl::Vec3d vjVecLast, gmtl::Vec3d vjVecNext );

    ///
    ///\return
    int getNumLocs();

    ///
    ///\return
    std::vector< std::vector< int > > getFlyThroughs();

    ///
    ///\return
    std::vector< int > getCompletionTest();

    ///If a quat is active this will move the cam to the next location
    void PreFrameUpdate();

    ///
    ///\return
    double GetQuatCamIncrementor();

    ///
    ///\return
    bool IsActive();

    ///
    unsigned int numQuatCams;
    ///
    unsigned int numFlyThroughs;
    ///
    unsigned int* numPointsInFlyThrough;

    ///
    int cfdId;
    ///
    int cfdIso_value;

protected:
    ///Update the gui with the new data
    void UpdateViewGUIPointData();

    ///Map of event handlers for texture-based vis
    std::map< std::string, ves::xplorer::event::EventHandler* > mEventHandlers;

private:
    ///
    bool activecam;
    ///
    bool _runFlyThrough;
    ///
    bool writeReadComplete;
    ///
    bool onMasterNode;

    ///
    unsigned int pointCounter;

    ///
    int run;
    ///
    int cam_id;
    ///
    int activeFlyThrough;
    ///
    int lastCommandId;
    ///
    int currentFrame;
    ///
    int writeFrame;

    ///
    double t;
    ///
    double rotvec[ 3 ];
    ///
    double angle;
    ///
    double movementIntervalCalc;
    ///
    double movementSpeed;

    ///
    std::string _param;
    ///
    std::string quatCamFileName;
    ///
    std::string quatCamDirName;

    ///
    std::vector< cfdQuatCam* > QuatCams;
    ///
    std::vector< std::vector< int > > flyThroughList;
    ///
    std::vector< int > completionTest;

    ///
    vpr::Timer* frameTimer;

    ///
    cfdQuatCam* thisQuatCam;

    ///
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > _worldDCS;

};
} //end xplorer
} //end ves

#endif //CFD_QUAT_CAM_HANDLER_H
