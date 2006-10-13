#include "VE_Xplorer/SceneGraph/NURBS/NURBSNode.h"
#include "VE_Xplorer/SceneGraph/NURBS/NSurface.h"
#include "VE_Xplorer/SceneGraph/NURBS/ControlPoint.h" 
#include <osg/Drawable>
#include <osg/BoundingBox>
#include <osg/BoundingSphere>
#include <osg/ShadeModel>
#include <osg/Material>
#include <iostream>

using namespace NURBS;
namespace NURBS{
/////////////////////////////////////////////////////////////
///Class for the control mesh drawable                     //
/////////////////////////////////////////////////////////////
class VE_NURBS_EXPORTS NURBSControlMesh: public osg::Drawable
{
public:
   ///Constructor
   NURBSControlMesh(){}

   NURBSControlMesh(std::vector<NURBS::ControlPoint>* controlPoints, 
                    unsigned int numU,
                    unsigned int numV,bool isSurface=false) 
   {
      _controlPoints = controlPoints;
      _numUControlPoints = numU;
      _numVControlPoints = numV;
      _isSurface = isSurface;
   }

   ///Copy constructor
   NURBSControlMesh(const NURBSControlMesh& controlMesh,
                    const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY):
                    osg::Drawable(controlMesh,copyop)
   {
      _controlPoints = controlMesh._controlPoints;
      _numUControlPoints = controlMesh._numUControlPoints;
      _numVControlPoints = controlMesh._numVControlPoints;
      _isSurface = controlMesh._isSurface;
   }

   META_Object(VE_NURBS,NURBS::NURBSControlMesh)

   ///Set the control points
   void SetControlPoints(std::vector<NURBS::ControlPoint> controlPoints,
                         unsigned int numU,
                         unsigned int numV,bool isSurface=false)
   {
      _controlPoints = &controlPoints;
      _numUControlPoints = numU;
      _numVControlPoints = numV;
      _isSurface = isSurface;
   }
   // the draw immediate mode method is where the OSG wraps up the drawing of
   // of OpenGL primitives.
   virtual void drawImplementation(osg::State& currentState) const;

   // we need to set up the bounding box of the data too, so that the scene graph knows where this
   // objects is, for both positioning the camera at start up, and most importantly for culling.
   virtual osg::BoundingBox computeBound() const;
   
protected:
   ///Destructor
   virtual ~NURBSControlMesh(){}

   ///Draw the u iso-curves
   void _drawUCurves()const;
   
   ///Draw the v iso-curves
   void _drawVCurves()const;

   ///Draw the UV Control Points
   void _drawUVPoints()const;

   bool _isSurface;///<Flag for determining NURBS type.
   unsigned int _numUControlPoints;///<The number of control points in U direction.
   unsigned int _numVControlPoints;///<The number of control points in V direction.
   std::vector<NURBS::ControlPoint>* _controlPoints;///<The control points
};
}
//////////////////////////////////////////
void NURBSControlMesh::_drawUCurves()const
{
   //u iso-curves
   for(unsigned int u = 0; u < _numUControlPoints; u++)
   {
      glBegin(GL_LINE_STRIP);
      for(unsigned int v = 0; v < _numVControlPoints; v++)
      {
         //osg::Vec3 nextPoint;
         glVertex3f(_controlPoints->at(u*_numVControlPoints + v).X(),
                    _controlPoints->at(u*_numVControlPoints + v).Y(),
                    _controlPoints->at(u*_numVControlPoints + v).Z());
      }
      glEnd();
   }
}
//////////////////////////////////////////
void NURBSControlMesh::_drawVCurves()const
{
   //v iso-curves
   for(unsigned int v = 0; v < _numVControlPoints; v++)
   {
      glBegin(GL_LINE_STRIP);
      for(unsigned int u = 0; u < _numUControlPoints; u++)
      {
         glVertex3f(_controlPoints->at(u*_numVControlPoints + v).X(),
                    _controlPoints->at(u*_numVControlPoints + v).Y(),
                    _controlPoints->at(u*_numVControlPoints + v).Z());
      }
      glEnd();
        
   }
}
//////////////////////////////////////////
void NURBSControlMesh::_drawUVPoints()const
{
   //u iso-curves     
   glEnable(GL_POINT_SMOOTH);
   glPointSize(5.0);
   glBegin(GL_POINTS);

   for(unsigned int v = 0; v < _numVControlPoints; v++)
   {
      for(unsigned int u = 0; u < _numUControlPoints; u++)
      {
         glVertex3f(_controlPoints->at(v*_numUControlPoints + u).X(),
                    _controlPoints->at(v*_numUControlPoints + u).Y(),
                    _controlPoints->at(v*_numUControlPoints + u).Z());
      }
   }
   
   glEnd();
   glDisable(GL_POINT_SMOOTH);
}
////////////////////////////////////////////////////////////////////////
void NURBSControlMesh::drawImplementation(osg::State& currentState)const
{
   if(_isSurface)
   {
      _drawUCurves();
      _drawVCurves();
   }
   else
   {
      if(_numUControlPoints > _numVControlPoints)
      {
         _drawVCurves();
      }
      else
      {
         _drawUCurves();
      }
   }
   _drawUVPoints();
}
//////////////////////////////////////////////////////
osg::BoundingBox NURBSControlMesh::computeBound()const
{
   osg::BoundingBox bbox;
   if(_controlPoints->size())
   {
      unsigned int nControlPoints = _controlPoints->size();
      for(unsigned int i = 0; i < nControlPoints; i++)
      {
         bbox.expandBy(_controlPoints->at(i).X(),_controlPoints->at(i).Y(),_controlPoints->at(i).Z());
      }
   }
   return bbox;
}
namespace NURBS
{
////////////////////////////////////////////////////////////////////
///Class for the tessellated surface mesh drawable                //
////////////////////////////////////////////////////////////////////
class VE_NURBS_EXPORTS NURBSTessellatedSurface: public osg::Drawable
{
public:
   ///Constructor
   NURBSTessellatedSurface(NURBS::NURBSObject* nurbsObject=0)
   {
      _nurbsObject = nurbsObject;
   }

   ///Copy constructor
   NURBSTessellatedSurface(const NURBSTessellatedSurface& tessSurf,
                           const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY):
                           osg::Drawable(tessSurf,copyop)
   {
      _nurbsObject = tessSurf._nurbsObject;
   }

   META_Object(VE_NURBS,NURBS::NURBSTessellatedSurface)

   // the draw immediate mode method is where the OSG wraps up the drawing of
   // of OpenGL primitives.
   virtual void drawImplementation(osg::State&) const;

   // we need to set up the bounding box of the data too, so that the scene graph knows where this
   // objects is, for both positioning the camera at start up, and most importantly for culling.
   virtual osg::BoundingBox computeBound() const;
   
protected:
   ///Destructor
   virtual ~NURBSTessellatedSurface(){}

   ///Tessellate the surface
   void _tessellateSurface()const;

   ///Tessellate the curve
   void _tessellateCurve() const;

   //Calculate the normal on the surface at point
   //Should this be in the NURBS::NURBSSurface class?
   osg::Vec3 _calculateSurfaceNormalAtPoint(unsigned int index)const;

   NURBS::NURBSObject* _nurbsObject;///The NURBS representation
};
}
///////////////////////////////////////////////////////////////////////////////
void NURBSTessellatedSurface::drawImplementation(osg::State& currentState)const
{
   if(_nurbsObject->GetType() == NURBSObject::Surface)
   {
      _tessellateSurface();
   }
   else
   {
      _tessellateCurve();
   }
}
/////////////////////////////////////////////////////////////
osg::BoundingBox NURBSTessellatedSurface::computeBound()const
{
   osg::BoundingBox bbox;
   if(_nurbsObject)
   {
      std::vector<NURBS::ControlPoint> ctPts = _nurbsObject->ControlPoints();
      unsigned int nControlPoints = ctPts.size();
      for(unsigned int i = 0; i < nControlPoints; i++)
      {
         bbox.expandBy(ctPts[i].X(),ctPts[i].Y(),ctPts[i].Z());
      }
   }
   return bbox;
}
//////////////////////////////////////////////////   
///Tessellate the surface                       //
//////////////////////////////////////////////////
void NURBSTessellatedSurface::_tessellateSurface()const
{
   if(_nurbsObject->InterpolatedPoints().empty())
   {
      std::cout<<"Invalid NURBSObject!!"<<std::endl;
      std::cout<<"NURBSRenderer::_tessellateSurface()"<<std::endl;
      return;
   }

   unsigned int nUPoints = _nurbsObject->NumInterpolatedPoints("U");
   unsigned int nVPoints = _nurbsObject->NumInterpolatedPoints("V");

   for(unsigned int u = 0; u </*2;*/ nUPoints - 1;u++)
   {
      //new tristrip
      glBegin(GL_TRIANGLE_STRIP);
      //Handle the special case for the first triangle in the strip
      
      //bottom corner vert
      glNormal3fv(_calculateSurfaceNormalAtPoint(u*nVPoints).ptr());
      glVertex3f(_nurbsObject->InterpolatedPoints().at(u*nVPoints).X(),
                 _nurbsObject->InterpolatedPoints().at(u*nVPoints).Y(),
                 _nurbsObject->InterpolatedPoints().at(u*nVPoints).Z());

      //right corner vert
      glNormal3fv(_calculateSurfaceNormalAtPoint((u+1)*nVPoints).ptr());
      glVertex3f(_nurbsObject->InterpolatedPoints().at((u+1)*nVPoints).X(),
                 _nurbsObject->InterpolatedPoints().at((u+1)*nVPoints).Y(),
                 _nurbsObject->InterpolatedPoints().at((u+1)*nVPoints).Z());

      //next top vert 
      glNormal3fv(_calculateSurfaceNormalAtPoint(u*nVPoints+1).ptr());
      glVertex3f(_nurbsObject->InterpolatedPoints().at((u)*nVPoints + 1).X(),
                 _nurbsObject->InterpolatedPoints().at((u)*nVPoints + 1).Y(),
                 _nurbsObject->InterpolatedPoints().at((u)*nVPoints + 1).Z());

      //interior points
      for(unsigned int v = 1; v < nVPoints - 1; v++)
      {
         glNormal3fv(_calculateSurfaceNormalAtPoint((u+1)*nVPoints+v).ptr());
         glVertex3f(_nurbsObject->InterpolatedPoints().at((u+1)*nVPoints + v).X(),
                    _nurbsObject->InterpolatedPoints().at((u+1)*nVPoints + v).Y(),
                    _nurbsObject->InterpolatedPoints().at((u+1)*nVPoints + v).Z());

         glNormal3fv(_calculateSurfaceNormalAtPoint((u)*nVPoints+(v+1)).ptr());
         glVertex3f(_nurbsObject->InterpolatedPoints().at(u*nVPoints + (v+1)).X(),
                    _nurbsObject->InterpolatedPoints().at(u*nVPoints + (v+1)).Y(),
                    _nurbsObject->InterpolatedPoints().at(u*nVPoints + (v+1)).Z());
      }
      //handle last point
      glNormal3fv(_calculateSurfaceNormalAtPoint((u+1)*nVPoints+(nVPoints - 1)).ptr());
      glVertex3f(_nurbsObject->InterpolatedPoints().at((u+1)*nVPoints + nVPoints - 1).X(),
                 _nurbsObject->InterpolatedPoints().at((u+1)*nVPoints + nVPoints - 1).Y(),
                 _nurbsObject->InterpolatedPoints().at((u+1)*nVPoints + nVPoints - 1).Z());
      glEnd();
   }
}
////////////////////////////////////////////////
///Tessellate the curve                       //
////////////////////////////////////////////////
void NURBSTessellatedSurface::_tessellateCurve()const
{
   if(_nurbsObject->InterpolatedPoints().empty())
   {
      std::cout<<"Invalid NURBSObject!!"<<std::endl;
      std::cout<<"NURBSTessellatedSurface::_tessellateCurve()"<<std::endl;
      return;
   }
   unsigned int nUPoints = _nurbsObject->NumInterpolatedPoints("U");
   
   //new linestrip
   glBegin(GL_LINE_STRIP);
   for(unsigned int u = 0; u < nUPoints; u++)
   {
      glVertex3f(_nurbsObject->InterpolatedPoints().at(u).X(),
                 _nurbsObject->InterpolatedPoints().at(u).Y(),
                 _nurbsObject->InterpolatedPoints().at(u).Z());
   }
   glEnd();
}	
///////////////////////////////////////////////////////////////////////////
osg::Vec3 NURBSTessellatedSurface::_calculateSurfaceNormalAtPoint(unsigned int index)const 
{
   osg::Vec3 normal(0,1,0);
   if(_nurbsObject->GetMinimumDegree() > 1)
   //if(_nurbsObject->GetType() == NURBSObject::Surface)
   {
      NURBS::NURBSSurface* surface = dynamic_cast<NURBS::NURBSSurface*>(_nurbsObject);
      
      NURBS::Point dSdV = surface->GetSurfaceDerivatives()[1][0].at(index);
      NURBS::Point dSdU = surface->GetSurfaceDerivatives()[0][1].at(index);
      NURBS::Point cross = dSdU^dSdV; 
      normal.set(cross.X(),cross.Y(),cross.Z());
      normal.normalize();
   }
   return normal;
}

////////////////////////////////////////////////
///Constructor                                //
////////////////////////////////////////////////
NURBSNode::NURBSNode(NURBS::NURBSObject* object)
{
   _nurbsObject = object;
   if(_nurbsObject)
   {
      
      _controlMeshGeode = new osg::Geode();

      _controlMeshDrawable = new NURBS::NURBSControlMesh(&_nurbsObject->ControlPoints(),
                                                         _nurbsObject->NumControlPoints("U"),
                                                         _nurbsObject->NumControlPoints("V"),
                                (_nurbsObject->GetType()== NURBS::NURBSObject::Surface)?true:false);

      _controlMeshGeode->addDrawable(_controlMeshDrawable.get());
      _controlMeshGeode->getOrCreateStateSet()->setMode(GL_LIGHTING,osg::StateAttribute::OFF);
      osg::ref_ptr<osg::Material> yellow = new osg::Material();
      yellow->setDiffuse(osg::Material::FRONT_AND_BACK,osg::Vec4(1,1,0,0));
      _controlMeshGeode->getOrCreateStateSet()->setAttribute(yellow.get());
      addChild(_controlMeshGeode.get());

      _triangulatedSurfaceGeode = new osg::Geode();
      _triangulatedSurfaceDrawable = new NURBS::NURBSTessellatedSurface(_nurbsObject);
      _triangulatedSurfaceGeode->addDrawable(_triangulatedSurfaceDrawable.get());

      osg::ref_ptr<osg::ShadeModel> shadeModel = new osg::ShadeModel();
      shadeModel->setMode(osg::ShadeModel::SMOOTH);

      osg::ref_ptr<osg::StateSet> surfaceState = _triangulatedSurfaceGeode->getOrCreateStateSet();
      surfaceState->setAttributeAndModes(shadeModel.get(),osg::StateAttribute::ON);
      
      addChild(_triangulatedSurfaceGeode.get());
   }
}
///////////////////////////////
///Destructor                //
///////////////////////////////
NURBSNode::~NURBSNode()
{
}
/////////////////////////////////////////////
///Show the triangulated wireframe surface //
/////////////////////////////////////////////
void NURBSNode::ViewWireframe(bool trueFalse)
{
   _wireframeView = trueFalse;
}
/////////////////////////////////////////
///Get the original surface            //
/////////////////////////////////////////
NURBS::NURBSObject* NURBSNode::GetNURBS()
{
   return _nurbsObject;
}
///////////////////////////////////////////////
///Get the osg::Geometry for the surface     //
///////////////////////////////////////////////
osg::Geode* NURBSNode::GetTriangulatedSurface()
{
   if(_triangulatedSurfaceGeode.valid())
   {
      return _triangulatedSurfaceGeode.get();
   }
   return 0;
}
///////////////////////////////////////
///Get the control mesh              //
///////////////////////////////////////
osg::Geode* NURBSNode::GetControlMesh()
{
   if(_controlMeshGeode.valid())
   {
      return _controlMeshGeode.get();
   }
   return 0;
}

////////////////////////////////////
///Draw the control mesh          //   
////////////////////////////////////
void NURBSNode::_updateControlMesh()
{
}
///////////////////////////////////////////////////////////////////////
///Calculate the surface normal at a point                           //
///////////////////////////////////////////////////////////////////////
osg::Vec3 NURBSNode::_calculateSurfaceNormalAtPoint(unsigned int index)
{
   return osg::Vec3(0.,1.,0.);
}
///////////////////////////////////////////////
osg::BoundingSphere NURBSNode::computeBound()const
{
   if(_controlMeshDrawable.valid())
   {
      return osg::BoundingSphere(_controlMeshDrawable->computeBound());
   }
   return osg::BoundingSphere();
}

