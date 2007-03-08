#ifndef WAND_H
#define WAND_H

/*!\file Wand.h
Wand API
*/
/*!\class VE_XPlorer::Wand
* 
*/
#include <gmtl/Vec.h>
#include <gmtl/Matrix.h>
#include <gadget/Type/PositionInterface.h>
#include <gadget/Type/DigitalInterface.h>

namespace VE_SceneGraph
{
   class DCS;
}

namespace VE_XML
{
   class Command;
}

#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/XplorerHandlers/Device.h"

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS Wand : public Device
{
public:
	Wand();
	~Wand();

   ///Initialize some variables in the class
   void Initialize( void );
	virtual void UpdateNavigation();
	virtual void UpdateSelection();
   ///bool to set the rotation method
   void SetHeadRotationFlag( int );
   ///New function for testing the new VECommand structure
   void SetVECommand( VE_XML::Command* veCommand );
   ///accessor to set initial world position
   void SetInitialWorldPosition( float* translate, float* rotate, float* scale );
   ///Get the current direction of the wand
   void UpdateDir( void );
   ///Update the juggler reference frame wand position  
   void UpdateLoc( void );
      
private:
   gadget::DigitalInterface digital[6];
   gadget::DigitalInterface IHdigital[10];
   gadget::DigitalInterface flyThrough[4];
   int buttonData[ 6 ];
   // x, y, and z translation of objects in world coordinates.
   // Variables only used in preFrame
   //double * currentWandDirection;
   int cfdId;
   int cfdIso_value;
   osg::ref_ptr< VE_SceneGraph::DCS > worldDCS;
   
   double worldTrans[ 3 ];
   float worldRot[ 3 ];
   
   /*!
      VR Juggler's wand positional interface.
    */
   //vjPosInterface wand;
   gadget::PositionInterface  wand;
   gadget::PositionInterface  head;
   //! VR Juggler
   /*!
   VR Juggler's vector math function.
   */
   gmtl::Vec3f  vjVec;
   gmtl::Vec3f  LastVec;
   //! VR Juggler
   /*!
      VR Juggler's matrix math function.
    */
   gmtl::Matrix44f vjMat;
   gmtl::Matrix44f vjHeadMat;
   //! Wand object
   /*!
      Location of the wand with respect to the virtual space.
    */
   double loc[3];
   //! Wand object
   /*!
      Direction of the wand.
    */
   double dir[3];
   //! Virtual environment object(s)
   /*!
      Location of the objects with respect to the virtual space.
    */
   double worldLoc[3];
   //! Cursor object(s)
   /*!
      Location of the cursor with respect to the virtual space.
    */
   double cursorLoc[3];
   //! Data set object(s)
   /*!
      Location with respect to data set (the actual location to interact with data).
    */
   float objLoc[3];
   //! Cursor object(s)
   /*!
      Cursor length.
    */
   float cursorLen;
   //! Wand object
   /*!
      Displacement of the objects in virtual space.
    */
   float dObj;
   
   float translationStepSize;
   float rotationStepSize;
   
   int rotationFlag;
   
   // class used to store xml command
   VE_XML::Command* command;
   // data storage for initial world dcs location
   float initialTranslate[ 3 ];
   float initialRotate[ 3 ];   
};
}

#endif //WAND_H