#include "VE_Xplorer/SceneGraph/CADCombo.h"

#include "VE_Xplorer/SceneGraph/DCS.h"
#include "VE_Xplorer/SceneGraph/Node.h"
#include "VE_Xplorer/SceneGraph/SceneNode.h"
#include "VE_Xplorer/SceneGraph/ModelOccluder.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#ifdef _PERFORMER
#include <Performer/pr/pfFog.h>
#elif _OSG
#include "VE_Xplorer/SceneGraph/PhysicsSimulator.h"

#include <osg/Fog>
#include <osg/Node>
#include <osg/Group>
#include <osg/MatrixTransform>
#include <osg/TriangleIndexFunctor>

class TriIndexFunc
{
public:
   TriIndexFunc(){;}
   ~TriIndexFunc(){;}

   void inline operator()(unsigned int pos1,unsigned int pos2,unsigned int pos3)
   {
      triangleIndex.push_back(pos1);
      triangleIndex.push_back(pos2);
      triangleIndex.push_back(pos3);
   }

   std::vector<osg::Vec3> vertexIndex;
   std::vector<unsigned int> triangleIndex;
};
#endif

//C/C++ libraries
#include <cassert>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
File::File(std::string geomFile,VE_SceneGraph::DCS* worldDCS,bool isStream)
{
   //Need to fix this and move some code to Node
   //Leave some code here no more FILEInfo
   this->DCS=new VE_SceneGraph::DCS();
   this->node=new VE_SceneGraph::Node();

   //this->node->LoadFile(geomFile.c_str(),isStream);
   fileName.assign(geomFile);
   this->DCS->AddChild( this->node.get() );
   worldDCS->AddChild( this->DCS.get() );

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
File::~File()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
std::string File::GetFilename()
{
   return fileName;
}
////////////////////////////////////////////////////////////////////////////////
void File::SetFILEProperties( int color, int trans, float* stlColor )
{
   this->color = color;
   this->_colorFlag = color;
   this->transparent = trans;
   this->stlColor[ 0 ] = stlColor[ 0 ];
   this->stlColor[ 1 ] = stlColor[ 1 ];
   this->stlColor[ 2 ] = stlColor[ 2 ];
}
////////////////////////////////////////////////////////////////////////////////
int File::GetTransparentFlag()
{
   return transparent;
}
////////////////////////////////////////////////////////////////////////////////
void File::Initialize( float op_val )
{
   this->op = op_val;
   setOpac( op_val );
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::Node* File::GetNode()
{
   return this->node.get();
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* File::GetDCS()
{
   return this->DCS.get();
}
////////////////////////////////////////////////////////////////////////////////
btRigidBody* File::GetRidgidBody()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
float File::getOpacity()
{
   return this->op;
}
////////////////////////////////////////////////////////////////////////////////
void File::setOpac(float op_val)
{
   this->op = op_val;
   //this->node->SetNodeProperties( _colorFlag, op, stlColor );

   #ifdef _PERFORMER
      this->node->pfTravNodeMaterial( this->node->GetRawNode() );
   #elif _OSG
      //node->TravNodeMaterial(node->GetRawNode());
   #endif
}
////////////////////////////////////////////////////////////////////////////////
void File::setFog(double dist)
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
      //this->node->TravNodeFog( this->node->GetRawNode(), fog );
   #endif
}
////////////////////////////////////////////////////////////////////////////////
/// Functions taken from module geometry for future merging
void File::SetRGBAColorArray(double* color)
{
   for(int i=0;i<4;i++)
   {
      this->_rgba[i] = color[i];
   }
   vprDEBUG(vesDBG,2) << " Color ModuleGeometry: " << this->_rgba[ 0 ]  << " : " <<  this->_rgba[ 1 ]  <<  " : " << this->_rgba[ 2 ]  << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void File::GetColorArray()
{
   vprDEBUG(vesDBG,2) << " Color ModuleGeometry: " << this->_rgba[ 0 ]  << " : " <<  this->_rgba[ 1 ]  <<  " : " << this->_rgba[ 2 ]  << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void File::SetTransparencyFlag( bool x )
{
   this->_transparencyFlag = x;
}
////////////////////////////////////////////////////////////////////////////////
void File::SetOpacity( float x )
{
   this->_opacityLevel = x;
}
////////////////////////////////////////////////////////////////////////////////
void File::SetColorFlag( int x )
{
   this->_colorFlag = x;
}
////////////////////////////////////////////////////////////////////////////////
int File::GetColorFlag()
{
   return this->_colorFlag;
}
////////////////////////////////////////////////////////////////////////////////
void File::SetModuleName( std::string filename )
{
   this->_moduleName = filename;
}
////////////////////////////////////////////////////////////////////////////////
std::string File::GetModuleName()
{
   return this->_moduleName;
}
////////////////////////////////////////////////////////////////////////////////
void File::SetGeometryFilename( std::string filename )
{
   this->_filename = filename;
   this->node = new VE_SceneGraph::Node();
   //this->node->LoadFile( (char*)this->_filename.c_str() );
   // Need to fix this
   //this->AddChild( (SceneNode*)this->_node );
   std::cout << "ModuleGeometry load geometry : " << _filename << std::endl;

   // Need to fix this
   //this->_masterNode->AddChild( this );   
}
////////////////////////////////////////////////////////////////////////////////
void File::Update()
{
   std::cout << "Update Filename : " << this->_filename << std::endl
               << "trans : " << this->_transparencyFlag << std::endl
               << "op : " << this->_opacityLevel << std::endl
               << "color : " << this->_colorFlag << std::endl;
   // Fix this later to call traverser function
   //this->_node->SetColorOfGeometry( this->_node );
}
////////////////////////////////////////////////////////////////////////////////

