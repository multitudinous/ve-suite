#include "VE_Xplorer/SceneGraph/NURBS/NURBSRenderer.h"
#include "VE_Xplorer/SceneGraph/NURBS/NSurface.h"
#include "VE_Xplorer/SceneGraph/NURBS/ControlPoint.h"
#include <osgUtil/SmoothingVisitor>
#include <osgUtil/Optimizer>
#include <osg/Geometry>
#include <osg/PolygonMode>
#include <osg/Point>
#include <osg/Geode>
#include <osg/Array>
#include <osg/ShadeModel>
#include <osg/Material>
#include <iostream>

using namespace NURBS;
////////////////////////////////////////////////////////////////////
///Constructor                                                    //  
////////////////////////////////////////////////////////////////////
NURBSRenderer::NURBSRenderer(NURBS::NURBSObject* object)
{
   _nurbsObject = 0;
   _nurbsObject = object;
   _retessellate = true;
   _wireframeView = false;
}
/////////////////////////////////////////////
///Destructor                              //
/////////////////////////////////////////////
NURBSRenderer::~NURBSRenderer()
{
}
////////////////////////////////////////////////////////
void NURBSRenderer::ViewWireframe(bool trueFalse)
{
   _wireframeView = trueFalse;
}
////////////////////////////////////////////////////////////////////
void NURBSRenderer::SetNURBSObject(NURBS::NURBSObject* object)
{
   _nurbsObject = object;
}
//////////////////////////////////////////////////////////
osg::Geode* NURBSRenderer::GetTriangulatedSurface()
{
   if(_retessellate)
   {
      if(!_nurbsObject)
      {
         std::cout<<"Invalid NURBSObject!!"<<std::endl;
         std::cout<<"NURBSRenderer::_updateControlMesh()"<<std::endl;
         return 0;
      }
      if(_nurbsObject->ControlPoints().empty())
      {
         std::cout<<"Invalid NURBSObject!!"<<std::endl;
         std::cout<<"NURBSRenderer::_updateControlMesh()"<<std::endl;
         return 0;
      }

      if(!_controlMesh.valid())
      {
         _controlMesh = new osg::Geode();
      }

      if(_nurbsObject->GetType() == NURBSObject::Surface)
      {
         _tessellateSurface();
      }
      else
      {
         _tessellateCurve();
      }
      _updateControlMesh();
   }
   return _triangulatedSurface.get();
}
/////////////////////////////////////////////////////
osg::Geode* NURBSRenderer::GetControlMesh()
{
   if(_controlMesh.valid())
      return _controlMesh.get();

   return 0;
}
///////////////////////////////////////////////
void NURBSRenderer::_updateControlMesh()
{
   unsigned int nVPoints = _nurbsObject->NumControlPoints("U");
   unsigned int nUPoints = _nurbsObject->NumControlPoints("V");
   
   //u iso-curves
   for(unsigned int u = 0; u < nUPoints; u++)
   {
      //new tristrip
      osg::ref_ptr<osg::Vec3Array> verts = new osg::Vec3Array();
      osg::ref_ptr<osg::Geometry> lineStrip = new osg::Geometry();
      osg::ref_ptr<osg::Geometry> vertStrip = new osg::Geometry();

      for(unsigned int v = 0; v < nVPoints; v++)
      {
         osg::Vec3 nextPoint;

         nextPoint.set(_nurbsObject->GetControlPoint(u*nVPoints + v).X(),
                       _nurbsObject->GetControlPoint(u*nVPoints + v).Y(),
                       _nurbsObject->GetControlPoint(u*nVPoints + v).Z());
         verts->push_back(nextPoint);
      }
      //set the verts for the line-strip
      lineStrip->setVertexArray(verts.get());
      lineStrip->setNormalBinding(osg::Geometry::BIND_PER_PRIMITIVE);
      lineStrip->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_STRIP,0,verts->size()));
      
      //set the verts for the actual points
      osg::ref_ptr<osg::StateSet> vertState = vertStrip->getOrCreateStateSet();
      osg::ref_ptr<osg::Point> pointSize = new osg::Point();
      pointSize->setSize(5.0);
      vertState->setAttribute(pointSize.get(),osg::StateAttribute::ON);
      vertStrip->setVertexArray(verts.get());
      vertStrip->setNormalBinding(osg::Geometry::BIND_PER_PRIMITIVE);
      vertStrip->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::POINTS,0,verts->size()));

      _controlMesh->addDrawable(lineStrip.get());
      _controlMesh->addDrawable(vertStrip.get());
   }

   if(_nurbsObject->GetType() == NURBSObject::Surface)
   {
      //v iso-curves
      for(unsigned int v = 0; v < nVPoints; v++)
      {
         //new linestrip
         osg::ref_ptr<osg::Vec3Array> vertStrip = new osg::Vec3Array();
         osg::ref_ptr<osg::Geometry> lineStrip = new osg::Geometry();
         for(unsigned int u = 0; u < nUPoints; u++)
         {
            osg::Vec3 nextPoint;

            nextPoint.set(_nurbsObject->GetControlPoint(u*nVPoints + v).X(),
                          _nurbsObject->GetControlPoint(u*nVPoints + v).Y(),
                          _nurbsObject->GetControlPoint(u*nVPoints + v).Z());
            vertStrip->push_back(nextPoint);
         }
         //set the verts for the tri-strip
         lineStrip->setVertexArray(vertStrip.get());
         lineStrip->setNormalBinding(osg::Geometry::BIND_PER_PRIMITIVE);
         lineStrip->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_STRIP,0,vertStrip->size()));
      
         _controlMesh->addDrawable(lineStrip.get());
      }
   }
   osg::ref_ptr<osg::Material> material = new osg::Material();
   material->setDiffuse(osg::Material::FRONT_AND_BACK,osg::Vec4(1,1,0,1));

   osg::ref_ptr<osg::StateSet> meshState = _controlMesh->getOrCreateStateSet();
   meshState->setAttributeAndModes(material.get(),osg::StateAttribute::ON);
   meshState->setMode(GL_LIGHTING,osg::StateAttribute::OFF);
   meshState->setMode(GL_AUTO_NORMAL,osg::StateAttribute::ON);

}
////////////////////////////////////
void NURBSRenderer::_tessellateCurve()
{
   if(_nurbsObject->InterpolatedPoints().empty())
   {
      std::cout<<"Invalid NURBSObject!!"<<std::endl;
      std::cout<<"NURBSRenderer::_tessellateCurve()"<<std::endl;
      return;
   }

   if(!_triangulatedSurface.valid())
   {
      _triangulatedSurface = new osg::Geode();
   }
   unsigned int nUPoints = _nurbsObject->NumInterpolatedPoints("U")+1;
   
   //new linestrip
   osg::ref_ptr<osg::Vec3Array> vertStrip = new osg::Vec3Array();
   osg::ref_ptr<osg::Geometry> lineStrip = new osg::Geometry();
   
   for(unsigned int u = 0; u < nUPoints; u++)
   {
      osg::Vec3 nextPoint;
      nextPoint.set(_nurbsObject->InterpolatedPoints().at(u).X(),
                    _nurbsObject->InterpolatedPoints().at(u).Y(),
                    _nurbsObject->InterpolatedPoints().at(u).Z());
      vertStrip->push_back(nextPoint);
   }
   //set the verts for the tri-strip
   lineStrip->setVertexArray(vertStrip.get());
   lineStrip->setNormalBinding(osg::Geometry::BIND_OFF);
   lineStrip->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINE_STRIP,0,vertStrip->size()));
      
    _triangulatedSurface->addDrawable(lineStrip.get());
}
///////////////////////////////////////////////
void NURBSRenderer::_tessellateSurface()
{
   if(_nurbsObject->InterpolatedPoints().empty())
   {
      std::cout<<"Invalid NURBSObject!!"<<std::endl;
      std::cout<<"NURBSRenderer::_tessellateSurface()"<<std::endl;
      return;
   }

   if(!_triangulatedSurface.valid())
   {
      _triangulatedSurface = new osg::Geode();
   }
   unsigned int nVPoints = _nurbsObject->NumInterpolatedPoints("U")+1;
   unsigned int nUPoints = _nurbsObject->NumInterpolatedPoints("V")+1;

   for(unsigned int u = 0; u </*2;*/nUPoints - 1; u++)
   {
      //new tristrip
      osg::ref_ptr<osg::Vec3Array> vertStrip = new osg::Vec3Array();
      osg::ref_ptr<osg::Vec3Array> normals = new osg::Vec3Array();
      osg::ref_ptr<osg::Geometry> triStrip = new osg::Geometry();

      //Handle the special case for the first triangle in the strip
      osg::Vec3 rightCornerPoint;
      osg::Vec3 bottomCornerPoint;
      osg::Vec3 nextTopPoint;

      osg::Vec3 rightCornerNormal = _calculateSurfaceNormalAtPoint(u*nVPoints);
      osg::Vec3 bottomCornerNormal = _calculateSurfaceNormalAtPoint((u+1)*nVPoints);
      osg::Vec3 nextTopNormal = _calculateSurfaceNormalAtPoint(u*nVPoints+1);

      bottomCornerPoint.set(_nurbsObject->InterpolatedPoints().at(u*nVPoints).X(),
                            _nurbsObject->InterpolatedPoints().at(u*nVPoints).Y(),
                            _nurbsObject->InterpolatedPoints().at(u*nVPoints).Z());

      rightCornerPoint.set(_nurbsObject->InterpolatedPoints().at((u+1)*nVPoints).X(),
                           _nurbsObject->InterpolatedPoints().at((u+1)*nVPoints).Y(),
                           _nurbsObject->InterpolatedPoints().at((u+1)*nVPoints).Z());

      

      nextTopPoint.set(_nurbsObject->InterpolatedPoints().at((u)*nVPoints + 1).X(),
                       _nurbsObject->InterpolatedPoints().at((u)*nVPoints + 1).Y(),
                       _nurbsObject->InterpolatedPoints().at((u)*nVPoints + 1).Z());


      //add the first triangle to the strip
      vertStrip->push_back(bottomCornerPoint);
      vertStrip->push_back(rightCornerPoint);
      vertStrip->push_back(nextTopPoint);

      normals->push_back(rightCornerNormal);
      normals->push_back(bottomCornerNormal);
      normals->push_back(nextTopNormal);

      //interior points
      for(unsigned int v = 1; v < nVPoints - 1; v++)
      {
         osg::Vec3 nextBottomCornerPoint;
         osg::Vec3 oppositeTopCornerPoint;

         osg::Vec3 nextBottomCornerNormal = _calculateSurfaceNormalAtPoint((u+1)*nVPoints+v);
         osg::Vec3 oppositeTopCornerNormal = _calculateSurfaceNormalAtPoint((u)*nVPoints+(v+1));
     
         nextBottomCornerPoint.set(_nurbsObject->InterpolatedPoints().at((u+1)*nVPoints + v).X(),
                                   _nurbsObject->InterpolatedPoints().at((u+1)*nVPoints + v).Y(),
                                   _nurbsObject->InterpolatedPoints().at((u+1)*nVPoints + v).Z());

         oppositeTopCornerPoint.set(_nurbsObject->InterpolatedPoints().at(u*nVPoints + (v+1)).X(),
                                    _nurbsObject->InterpolatedPoints().at(u*nVPoints + (v+1)).Y(),
                                    _nurbsObject->InterpolatedPoints().at(u*nVPoints + (v+1)).Z());

         vertStrip->push_back(nextBottomCornerPoint);
         vertStrip->push_back(oppositeTopCornerPoint);

         normals->push_back(nextBottomCornerNormal);
         normals->push_back(oppositeTopCornerNormal);
      }
      //handle last point
      osg::Vec3 lastBottomPoint;
      lastBottomPoint.set(_nurbsObject->InterpolatedPoints().at((u+1)*nVPoints + nVPoints - 1).X(),
                          _nurbsObject->InterpolatedPoints().at((u+1)*nVPoints + nVPoints - 1).Y(),
                          _nurbsObject->InterpolatedPoints().at((u+1)*nVPoints + nVPoints - 1).Z());
      
      vertStrip->push_back(lastBottomPoint);

      
      osg::Vec3 lastBottomNormal = _calculateSurfaceNormalAtPoint((u+1)*nVPoints+(nVPoints - 1));
      normals->push_back(lastBottomNormal);

      //set the verts for the tri-strip
      triStrip->setVertexArray(vertStrip.get());
      triStrip->setNormalBinding(osg::Geometry::BIND_PER_VERTEX);
      triStrip->setNormalArray(normals.get());
      triStrip->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::TRIANGLE_STRIP,0,vertStrip->size()));
      
      _triangulatedSurface->addDrawable(triStrip.get());

   }
   
   if(_wireframeView)
   {
      osg::ref_ptr<osg::PolygonMode> polygonMode = new osg::PolygonMode;
      polygonMode->setMode(osg::PolygonMode::FRONT_AND_BACK,osg::PolygonMode::LINE);
      _triangulatedSurface->getOrCreateStateSet()->setAttributeAndModes(polygonMode.get(), osg::StateAttribute::ON);
   }

   osg::ref_ptr<osg::ShadeModel> shadeModel = new osg::ShadeModel();
   shadeModel->setMode(osg::ShadeModel::SMOOTH);

   osg::ref_ptr<osg::StateSet> surfaceState = _triangulatedSurface->getOrCreateStateSet();
   surfaceState->setAttributeAndModes(shadeModel.get(),osg::StateAttribute::ON);
   surfaceState->setMode(GL_NORMALIZE, osg::StateAttribute::ON);
   
   //surfaceState->setMode(GL_AUTO_NORMAL,osg::StateAttribute::ON);

   if(_triangulatedSurface.valid())
   {
      //osgUtil::SmoothingVisitor autoNormals;
      //autoNormals.apply(*_triangulatedSurface.get());

      //osgUtil::Optimizer optimizer;
      //optimizer.optimize(_triangulatedSurface.get(),osgUtil::Optimizer::ALL_OPTIMIZATIONS);
      
   }
   _retessellate = false;
}
///////////////////////////////////////////////////////////////////////////
osg::Vec3 NURBSRenderer::_calculateSurfaceNormalAtPoint(unsigned int index)
{
   osg::Vec3 normal(0,1,0);

   if(_nurbsObject->GetType() == NURBSObject::Surface)
   {
      try
      {
         NURBS::NURBSSurface* surface = dynamic_cast<NURBS::NURBSSurface*>(_nurbsObject);
      
         NURBS::Point dSdU = surface->GetSurfaceDerivatives()[1][0].at(index);
         NURBS::Point dSdV = surface->GetSurfaceDerivatives()[0][1].at(index);
         NURBS::Point cross = dSdV^dSdU; 
         normal.set(cross.X(),cross.Y(),cross.Z());
         normal.normalize();
      }
      catch(...)
      {
         std::cout<<"Invalid surface!!!"<<std::endl;
         std::cout<<"NURBSRenderer::_calculateSurfaceNormalAtPoint"<<std::endl;
      }
   }
   return normal;
}

