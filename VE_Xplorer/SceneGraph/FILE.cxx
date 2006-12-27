#include <cassert>

#include "VE_Xplorer/SceneGraph/FILE.h"

#include "VE_Xplorer/SceneGraph/DCS.h"
#include "VE_Xplorer/SceneGraph/Node.h"
#include "VE_Xplorer/SceneGraph/ModelOccluder.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#ifdef _PERFORMER
   #include <Performer/pr/pfFog.h>
#elif _OSG
   #include <osg/Fog>
   #include <osg/Node>
   #include <osg/Group>
   #include <osg/MatrixTransform>
#endif

#include <opal.h>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
FILE::FILE(std::string geomFile, VE_SceneGraph::DCS* worldDCS, bool isStream)
{
   // Need to fix this and move some code to Node
   // Leave some code here no more FILEInfo
   this->DCS=new VE_SceneGraph::DCS();
   this->node=new VE_SceneGraph::Node();  
   this->node->LoadFile(geomFile.c_str(),isStream);
   fileName.assign(geomFile);
   this->DCS->AddChild(this->node);
   worldDCS->AddChild(this->DCS);

   #ifdef _PERFORMER
      fog = new pfFog();
   #elif _OSG
      //setup occluder node
      //ModelOccluder occluder;
      //dynamic_cast< osg::MatrixTransform* >( this->DCS->GetRawNode() )->
      //            addChild( occluder.GetOccluderNode( node->GetRawNode() ).get() );
      //setup fog
      fog=new osg::Fog();
   #endif
}
////////////////////////////////////////////////////////////////////////////////
FILE::~FILE()
{
   vprDEBUG(vesDBG,2) << "FILE Destructor" << std::endl << vprDEBUG_FLUSH;

   delete this->DCS;

   #ifndef _OSG
      delete this->node;
   #endif
      //delete fog;
}
////////////////////////////////////////////////////////////////////////////////
std::string FILE::GetFilename()
{
   return fileName;
}
////////////////////////////////////////////////////////////////////////////////
void FILE::SetFILEProperties( int color, int trans, float* stlColor )
{
   this->color = color;
   this->_colorFlag = color;
   this->transparent = trans;
   this->stlColor[ 0 ] = stlColor[ 0 ];
   this->stlColor[ 1 ] = stlColor[ 1 ];
   this->stlColor[ 2 ] = stlColor[ 2 ];
}
////////////////////////////////////////////////////////////////////////////////
int FILE::GetTransparentFlag()
{
   return transparent;
}
////////////////////////////////////////////////////////////////////////////////
void FILE::Initialize( float op_val )
{
   this->op = op_val;
   setOpac( op_val );
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::Node* FILE::GetNode()
{
   return this->node;
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* FILE::GetDCS()
{
   return this->DCS;
}
////////////////////////////////////////////////////////////////////////////////
float FILE::getOpacity()
{
   return this->op;
}
////////////////////////////////////////////////////////////////////////////////
void FILE::setOpac(float op_val)
{
   this->op = op_val;
   this->node->SetNodeProperties( _colorFlag, op, stlColor );

   #ifdef _PERFORMER
      this->node->pfTravNodeMaterial( this->node->GetRawNode() );
   #elif _OSG
      node->TravNodeMaterial(node->GetRawNode());
   #endif
}
////////////////////////////////////////////////////////////////////////////////
void FILE::setFog(double dist)
{
   #ifdef _PERFORMER
      fog->setColor( 0.6f, 0.6f, 0.6f);
      fog->setRange(0, dist);
      fog->setFogType(PFFOG_PIX_EXP2);
      this->node->pfTravNodeFog( this->node->GetRawNode(), fog );
   #elif _OSG
      fog->setMode( osg::Fog::EXP2 );
      fog->setDensity( 1 / ( dist / 2 ) );
      fog->setColor( osg::Vec4( 0.5f, 0.5f, 0.5f, 0.0f ) );
      //fog->setStart( 0.0f );
      //fog->setStart( dist + 100 );
      //fog->setEnd( dist + 200 );
      //fog->setFogCoordinateSource( );
      this->node->TravNodeFog( this->node->GetRawNode(), fog );
   #endif
}
////////////////////////////////////////////////////////////////////////////////
/// Functions taken from module geometry for future merging
void FILE::SetRGBAColorArray(double* color)
{
   for(int i=0;i<4;i++)
   {
      this->_rgba[i] = color[i];
   }
   vprDEBUG(vesDBG,2) << " Color ModuleGeometry: " << this->_rgba[ 0 ]  << " : " <<  this->_rgba[ 1 ]  <<  " : " << this->_rgba[ 2 ]  << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void FILE::GetColorArray()
{
   vprDEBUG(vesDBG,2) << " Color ModuleGeometry: " << this->_rgba[ 0 ]  << " : " <<  this->_rgba[ 1 ]  <<  " : " << this->_rgba[ 2 ]  << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void FILE::SetTransparencyFlag( bool x )
{
   this->_transparencyFlag = x;
}
////////////////////////////////////////////////////////////////////////////////
void FILE::SetOpacity( float x )
{
   this->_opacityLevel = x;
}
////////////////////////////////////////////////////////////////////////////////
void FILE::SetColorFlag( int x )
{
   this->_colorFlag = x;
}
////////////////////////////////////////////////////////////////////////////////
int FILE::GetColorFlag()
{
   return this->_colorFlag;
}
////////////////////////////////////////////////////////////////////////////////
void FILE::SetModuleName( std::string filename )
{
   this->_moduleName = filename;
}
////////////////////////////////////////////////////////////////////////////////
std::string FILE::GetModuleName()
{
   return this->_moduleName;
}
////////////////////////////////////////////////////////////////////////////////
void FILE::SetGeometryFilename( std::string filename )
{
   this->_filename = filename;
   this->_node = new VE_SceneGraph::Node();
   this->_node->LoadFile( (char*)this->_filename.c_str() );
   //this->_node->flatten( 0 );
   // Need to fix this
   //this->AddChild( (SceneNode*)this->_node );
   std::cout << "ModuleGeometry load geometry : " << _filename << std::endl;

   // Need to fix this
   //this->_masterNode->AddChild( this );   
}
////////////////////////////////////////////////////////////////////////////////
void FILE::Update()
{
   std::cout << "Update Filename : " << this->_filename << std::endl
               << "trans : " << this->_transparencyFlag << std::endl
               << "op : " << this->_opacityLevel << std::endl
               << "color : " << this->_colorFlag << std::endl;
   // Fix this later to call traverser function
   //this->_node->SetColorOfGeometry( this->_node );
}
////////////////////////////////////////////////////////////////////////////////

