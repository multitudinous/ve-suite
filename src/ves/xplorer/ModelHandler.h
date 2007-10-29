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
#ifndef CFD_MODELHANDLER_H
#define CFD_MODELHANDLER_H
/*!\file cfdModelHandler.h
cfdModelHandler API
*/
/*!\class VE_Xplorer::cfdModelHandler
* 
*/
#include <vpr/Util/Singleton.h>

#include <map>
#include <utility>

#include <ves/open/xml/CommandPtr.h>

namespace VE_Xplorer
{
    class cfdDataSet;
    class cfdModel;
    class cfdCommandArray;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
	class CADEntity;
}
}
}

namespace VE_EVENTS
{
   class EventHandler;
}

#ifdef _OSG
namespace ves
{
namespace xplorer
{
namespace volume
{
   class cfdTextureDataSet;
}
}
}
#endif
class vtkPolyData;

#include <vector>
#include <ves/VEConfig.h>

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdModelHandler //: public vpr::Singleton< cfdModelHandler >
{
private:
   // Required so that vpr::Singleton can instantiate this class.
   //friend class vpr::Singleton< cfdModelHandler >;
   //cfdModelHandler(const cfdModelHandler& o) { ; }
   //cfdModelHandler& operator=(const cfdModelHandler& o) { ; }
   cfdModelHandler( void );
   ~cfdModelHandler( void );
   vprSingletonHeader( cfdModelHandler );   
public:
   void Initialize( std::string );
   //void CleanUp( void );
   void InitScene( void );
   void PreFrameUpdate( void );

   void SetCommandArray( cfdCommandArray* );
   ///Set the active xml command
   ///\param inputCommand input xml command
   void SetXMLCommand( ves::open::xml::Command* inputCommand );
   ///Returns the current xml command
   ves::open::xml::Command* GetXMLCommand( void );
   //void CreateObjects( void );      
   void LoadSurfaceFiles( std::string );
   vtkPolyData* GetArrow( void );
   cfdModel* GetModel( int );
   void AddModel( cfdModel* );
   void RemoveModel( cfdModel* );
   cfdModel* GetActiveModel( void );
   void SetActiveModel( int modelNumber );
   int GetNumberOfModels( void );

   void ReadNNumberOfDataSets(  std::string, std::string );

   ///Get the scalar bar - may not be needed anymore
   //cfdScalarBarActor* GetScalarBar(void);
   ///Register CAD file with so that other models can copy files if needed
   ///\param tempEntity File to be registered
   void RegisterCADFile( ves::xplorer::scenegraph::CADEntity* tempEntity );
   ///Check and see if this cad file is already loaded
   ///\return The found cad file otherwise null
   ///\param filename The file name to grab
   ves::xplorer::scenegraph::CADEntity* IsCADFileLoaded( std::string filename );
   ///Reset the map holding cad references
   ///\param tempEntity CAD file to try and remove from the map
   void UnregisterCADFile( ves::xplorer::scenegraph::CADEntity* tempEntity );
#ifdef _OSG
   //texture manager access
   ///Should this call be in the texture manager singleton not modelhandler
   ves::xplorer::volume::cfdTextureDataSet* GetActiveTextureDataSet( void );
#endif
   bool GetVisOption();
protected:
   vtkPolyData* _GetArrowPolyData();
private:
   std::string _param;
   cfdDataSet* activeDataset;
   cfdCommandArray* commandArray;
   ves::open::xml::Command* activeCommand;
   ves::open::xml::Command* nullCommand;
   //cfdReadParam* _readParam;
   //cfdScalarBarActor* _scalarBar;
   cfdModel* _activeModel;

#ifdef _OSG
   ves::xplorer::volume::cfdTextureDataSet* _activeTDSet;
#endif
   bool tbased;
   vtkPolyData* arrow;
   std::vector< cfdModel* > _modelList;
   // Used to store data for multi-dataset functions
   std::string oldDatasetName;//[256];

    ///The event handler for commands.
    std::map< std::string,VE_EVENTS::EventHandler*> _eventHandlers;
    ///This map connects filenames to GUIDs so that we can 
    ///figure out what CAD files should be copied
    std::multimap< std::string, ves::xplorer::scenegraph::CADEntity* > m_filenameToCADMap;
};
}
#endif
