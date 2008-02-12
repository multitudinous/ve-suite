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
#ifndef CREATE_VIS_OBJECT_EVENT_HANDLER_H
#define CREATE_VIS_OBJECT_EVENT_HANDLER_H

#include <ves/xplorer/event/EventHandler.h>

#include <string>
#include <vector>
#include <map>
#include <utility>

namespace ves
{
namespace xplorer
{
class cfdPolyData;
class cfdIsosurface;
class cfdPresetContour;
class cfdContours;
class cfdMomentum;
class cfdPresetMomentum;
class cfdMomentums;
class cfdVector;
class cfdPresetVector;
class cfdVectors;
class cfdStreamers;
class cfdPolyData;
class cfdImage;
class cfdAnimatedImage;
class cfdAnimatedStreamlineCone;
class cfdContour;
class cfdObjects;
class cfdCursor;
class cfdGraphicsObject;
class cfdModel;
class cfdTextOutput;
class cfdObjects;
}
}

namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file CreateVisObjectEventHandler.h
  CreateVisObjectEventHandler API
  */
/*!\class CreateVisObjectEventHandler
 * Class for creating vis options.
 */
class VE_XPLORER_EXPORTS CreateVisObjectEventHandler: public EventHandler
{
public:
    ///Constructor
    CreateVisObjectEventHandler();

    ///Copy Constructor
    CreateVisObjectEventHandler( const CreateVisObjectEventHandler& rhs );

    ///Destructor
    virtual ~CreateVisObjectEventHandler();

    ///Equal operator
    CreateVisObjectEventHandler& operator=( const CreateVisObjectEventHandler& rhs );

    ///Set the Model.
    ///\param model The Model to execute the Command on\n.
    ///Default uses the active Model from ModelHandler\n
    ///Otherwise, the Model passed in is used.
    void SetGlobalBaseObject( ves::xplorer::GlobalBase* model = 0 );

    ///Exectute the event
    ///\param xmlObject The current xmlObject event.
    void Execute( ves::open::xml::XMLObjectPtr command );

private:
    ///Set the active dataset for the cfdobject to work on
    void SetActiveDataSet( ves::open::xml::XMLObjectPtr xmlObject );
    ///Set the active scalar and appropriate range
    void SetActiveScalarAndRange( ves::open::xml::XMLObjectPtr xmlObject );
    ///Set access to the active sclalt range
    void SetActiveVector( ves::open::xml::XMLObjectPtr xmlObject );
    ///Set the active cfd object based on the command
    void SetActiveCfdObject( ves::open::xml::XMLObjectPtr xmlObject );

    //ves::xplorer::Model* _activeModel;///<The active cfdModel
    ves::xplorer::cfdObjects* activeObject;   ///<The active cfdObject
    ves::xplorer::cfdPolyData*         surface;///<A cfdObject
    ves::xplorer::cfdIsosurface*       isosurface;///<A cfdObject
    ves::xplorer::cfdContour*          contour;///<A cfdObject
    ves::xplorer::cfdPresetContour*    x_contour;///<A cfdObject
    ves::xplorer::cfdPresetContour*    y_contour;///<A cfdObject
    ves::xplorer::cfdPresetContour*    z_contour;///<A cfdObject
    ves::xplorer::cfdContours*         x_contours;///<A cfdObject
    ves::xplorer::cfdContours*         y_contours;///<A cfdObject
    ves::xplorer::cfdContours*         z_contours;///<A cfdObject
    ves::xplorer::cfdMomentum*         momentum;///<A cfdObject
    ves::xplorer::cfdPresetMomentum*   x_momentum;///<A cfdObject
    ves::xplorer::cfdPresetMomentum*   y_momentum;///<A cfdObject
    ves::xplorer::cfdPresetMomentum*   z_momentum;///<A cfdObject
    ves::xplorer::cfdMomentums*        x_momentums;///<A cfdObject
    ves::xplorer::cfdMomentums*        y_momentums;///<A cfdObject
    ves::xplorer::cfdMomentums*        z_momentums;///<A cfdObject
    ves::xplorer::cfdVector*           vector;///<A cfdObject
    ves::xplorer::cfdPresetVector*     x_vector;///<A cfdObject
    ves::xplorer::cfdPresetVector*     y_vector;///<A cfdObject
    ves::xplorer::cfdPresetVector*     z_vector;///<A cfdObject
    ves::xplorer::cfdVectors*          x_vectors;///<A cfdObject
    ves::xplorer::cfdVectors*          y_vectors;///<A cfdObject
    ves::xplorer::cfdVectors*          z_vectors;///<A cfdObject
    ves::xplorer::cfdStreamers*        streamlines;///<A cfdObject
    ves::xplorer::cfdPolyData*         particles;///<A cfdObject
    ves::xplorer::cfdImage*            image;///<A cfdObject
    ves::xplorer::cfdAnimatedImage*    animImg;///<A cfdObject
    ves::xplorer::cfdAnimatedStreamlineCone* animStreamer;///<A cfdObject
    //ves::xplorer::cfdTextOutput*       textOutput;///<A cfdObject

    // Vectors that will eventually be stored as maps
    // these hold all the objectsa for easy access and management
    //std::vector< ves::xplorer::cfdObjects* > dataList;
    //std::vector< ves::xplorer::GlobalBase* > commandList;
    std::map< std::pair< std::string, std::pair< std::string, std::string > > , ves::xplorer::cfdObjects* > visObjectMap;///<The container for all of the cfdObjects

};

}
}
}

#endif// VE_EVENT_HANDLER_H
