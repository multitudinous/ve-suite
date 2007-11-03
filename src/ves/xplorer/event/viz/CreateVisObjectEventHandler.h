/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CREATE_VIS_OBJECT_EVENT_HANDLER_H
#define CREATE_VIS_OBJECT_EVENT_HANDLER_H
/*!\file CreateVisObjectEventHandler.h
  CreateVisObjectEventHandler API
  */
/*!\class CreateVisObjectEventHandler
 * Class for creating vis options.
 */
#include <string>
#include <vector>
#include <map>
#include <utility>

namespace VE_Xplorer
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
   class cfdGlobalBase;
   class cfdObjects;
   class cfdCommandArray;
   class cfdCursor;
   class cfdGraphicsObject;
   class cfdModel;
   class cfdTextOutput;
   class cfdObjects;
   class cfdGlobalBase;
}

namespace VE_Xplorer
{
   class cfdModel;
   class cfdGlobalBase;
}
#include <ves/xplorer/event/EventHandler.h>
namespace ves
{
namespace xplorer
{
namespace event
{
class VE_XPLORER_EXPORTS CreateVisObjectEventHandler: public EventHandler
{
public:
   ///Constructor
   CreateVisObjectEventHandler();

   ///Copy Constructor
   CreateVisObjectEventHandler(const CreateVisObjectEventHandler& rhs);

   ///Destructor
   virtual ~CreateVisObjectEventHandler();

   ///Equal operator
   CreateVisObjectEventHandler& operator=(const CreateVisObjectEventHandler& rhs);

   ///Set the cfdModel.
   ///\param model The cfdModel to execute the Command on\n.
   ///Default uses the active cfdModel from cfdModelHandler\n
   ///Otherwise, the cfdModel passed in is used.
   void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model=0);

   ///Exectute the event
   ///\param xmlObject The current xmlObject event.
   void Execute( ves::open::xml::XMLObject* command); 

private:
   ///Set the active dataset for the cfdobject to work on
   void SetActiveDataSet( ves::open::xml::XMLObject* xmlObject );
   ///Set the active scalar and appropriate range
   void SetActiveScalarAndRange( ves::open::xml::XMLObject* xmlObject );
   ///Set access to the active sclalt range
   void SetActiveVector( ves::open::xml::XMLObject* xmlObject );
   ///Set the active cfd object based on the command
   void SetActiveCfdObject( ves::open::xml::XMLObject* xmlObject );

   //VE_Xplorer::cfdModel* _activeModel;///<The active cfdModel
   VE_Xplorer::cfdObjects* activeObject;   ///<The active cfdObject
   VE_Xplorer::cfdPolyData*         surface;///<A cfdObject
   VE_Xplorer::cfdIsosurface*       isosurface;///<A cfdObject
   VE_Xplorer::cfdContour*          contour;///<A cfdObject
   VE_Xplorer::cfdPresetContour*    x_contour;///<A cfdObject
   VE_Xplorer::cfdPresetContour*    y_contour;///<A cfdObject
   VE_Xplorer::cfdPresetContour*    z_contour;///<A cfdObject
   VE_Xplorer::cfdContours*         x_contours;///<A cfdObject
   VE_Xplorer::cfdContours*         y_contours;///<A cfdObject
   VE_Xplorer::cfdContours*         z_contours;///<A cfdObject
   VE_Xplorer::cfdMomentum*         momentum;///<A cfdObject
   VE_Xplorer::cfdPresetMomentum*   x_momentum;///<A cfdObject
   VE_Xplorer::cfdPresetMomentum*   y_momentum;///<A cfdObject
   VE_Xplorer::cfdPresetMomentum*   z_momentum;///<A cfdObject
   VE_Xplorer::cfdMomentums*        x_momentums;///<A cfdObject
   VE_Xplorer::cfdMomentums*        y_momentums;///<A cfdObject
   VE_Xplorer::cfdMomentums*        z_momentums;///<A cfdObject
   VE_Xplorer::cfdVector*           vector;///<A cfdObject
   VE_Xplorer::cfdPresetVector*     x_vector;///<A cfdObject
   VE_Xplorer::cfdPresetVector*     y_vector;///<A cfdObject
   VE_Xplorer::cfdPresetVector*     z_vector;///<A cfdObject
   VE_Xplorer::cfdVectors*          x_vectors;///<A cfdObject
   VE_Xplorer::cfdVectors*          y_vectors;///<A cfdObject
   VE_Xplorer::cfdVectors*          z_vectors;///<A cfdObject
   VE_Xplorer::cfdStreamers*        streamlines;///<A cfdObject
   VE_Xplorer::cfdPolyData*         particles;///<A cfdObject
   VE_Xplorer::cfdImage*            image;///<A cfdObject
   VE_Xplorer::cfdAnimatedImage*    animImg;///<A cfdObject
   VE_Xplorer::cfdAnimatedStreamlineCone* animStreamer;///<A cfdObject
   //VE_Xplorer::cfdTextOutput*       textOutput;///<A cfdObject

   // Vectors that will eventually be stored as maps
   // these hold all the objectsa for easy access and management
   //std::vector< VE_Xplorer::cfdObjects* > dataList;
   //std::vector< VE_Xplorer::cfdGlobalBase* > commandList;
   std::map< std::pair< std::string, std::pair< std::string, std::string > >, VE_Xplorer::cfdObjects* > visObjectMap;///<The container for all of the cfdObjects

};

}
}
}

#endif// VE_EVENT_HANDLER_H
