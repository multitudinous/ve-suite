// Name: VirtualPaint Demo
// HUD.cpp
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011

// STL includes
#include <string>
#include <iostream>
#include <sstream>
#include <osg/Geode>
#include <osgText/Font>
#include <osgText/Text>
#include <osg/Camera>

#include "HUD.h"

static osg::Camera* CreateHUDCamera(double left, double right, double bottom, double top);
static osgText::Text *CreateText(osgText::Font *font, float fontSize, const osg::Vec3 &pos, const std::string &content, osgText::TextBase::AlignmentType align = osgText::TextBase::BASE_LINE);

// a convenience so don't have to prefix STL stuff with "std::".
using namespace std;
// ditto osg
using namespace osg;

// all of the below will probably become some sort of object in the future

// globals for HUD updating
osg::ref_ptr<osgText::Text> textTarget;
osg::ref_ptr<osgText::Text> textDistance;
osg::ref_ptr<osgText::Text> textPitch;
osg::ref_ptr<osgText::Text> textRunThick;
osg::ref_ptr<osgText::Text> textViscosity;
osg::ref_ptr<osgText::Text> textFluidPressure;
osg::ref_ptr<osgText::Text> textOrifice;
osg::ref_ptr<osgText::Text> textFan;

// **************************************************************************

osg::Node *BuildHud(const Lesson &lesson, const PaintGun &gun)
{
	osg::ref_ptr<osg::Camera> hud = CreateHUDCamera(0, 1600, 0, 1200);
	if (hud.valid() == false)
		return 0;
	osg::ref_ptr<osgText::Font> font = osgText::readFontFile("fonts/arial.ttf");
	if (font.valid() == false)
		return 0;
	osg::ref_ptr<osg::Geode> textHolder = new osg::Geode;
	if (textHolder.valid() == false)
		return 0;
	// Add the HUD text.
	textHolder->addDrawable((textTarget = CreateText(font.get(), 20.0f, osg::Vec3(10.0f, 10.0f, 0.0f), "")).get());
	textHolder->addDrawable((textDistance = CreateText(font.get(), 20.0f, osg::Vec3(350.0f, 10.0f, 0.0f), "")).get());
	textHolder->addDrawable((textPitch = CreateText(font.get(), 20.0f, osg::Vec3(800.0f, 10.0f, 0.0f), "")).get());
	textHolder->addDrawable((textRunThick = CreateText(font.get(), 20.0f, osg::Vec3(1590.0f, 106.0f, 0.0f), "", osgText::TextBase::RIGHT_BASE_LINE)).get());
	textHolder->addDrawable((textViscosity = CreateText(font.get(), 20.0f, osg::Vec3(1590.0f, 82.0f, 0.0f), "", osgText::TextBase::RIGHT_BASE_LINE)).get());
	textHolder->addDrawable((textFluidPressure = CreateText(font.get(), 20.0f, osg::Vec3(1590.0f, 58.0f, 0.0f), "", osgText::TextBase::RIGHT_BASE_LINE)).get());
	textHolder->addDrawable((textOrifice = CreateText(font.get(), 20.0f, osg::Vec3(1590.0f, 34.0f, 0.0f), "", osgText::TextBase::RIGHT_BASE_LINE)).get());
	textHolder->addDrawable((textFan = CreateText(font.get(), 20.0f, osg::Vec3(1590.0f, 10.0f, 0.0f), "", osgText::TextBase::RIGHT_BASE_LINE)).get());
	// add the text to the hud camera.
	hud->addChild(textHolder.get());
   UpdateHud(lesson, gun);
	return hud.release();
}

// **************************************************************************

void UpdateHud(const Lesson &lesson, const PaintGun &gun)

{
   ostringstream formatter;
   formatter << "Target: " << lesson.GetTargetThicknessMin() << " to " << lesson.GetTargetThicknessMax() << " mils";
   textTarget->setText(formatter.str());
   formatter.str("");
   formatter << "Run Thickness: " << lesson.GetRunThickness();
   textRunThick->setText(formatter.str());
   formatter.str("");
   formatter << "Distance to Target: " << gun.GetDistance() << " inches";
   textDistance->setText(formatter.str());
   formatter.str("");
   formatter << "Pitch: " << gun.GetPitch();
   textPitch->setText(formatter.str());
   formatter.str("");
   formatter << "Viscosity: " << gun.GetViscosity() << " sec";
   textViscosity->setText(formatter.str());
   formatter.str("");
   formatter << "Fluid Pressure: " << gun.GetFluidPressure() << " PSI";
   textFluidPressure->setText(formatter.str());
   formatter.str("");
   formatter << "Orifice: " << gun.GetOrifice();
   textOrifice->setText(formatter.str());
   formatter.str("");
   formatter << "Fan. " << gun.GetFan() << " in";
   textFan->setText(formatter.str());
}

// **************************************************************************

osg::Camera *CreateHUDCamera(double left, double right, double bottom, double top)

{
	osg::ref_ptr<osg::Camera> camera = new osg::Camera;
	camera->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
	camera->setClearMask(GL_DEPTH_BUFFER_BIT);
	camera->setRenderOrder(osg::Camera::POST_RENDER);
	camera->setAllowEventFocus(false);
	camera->setProjectionMatrix(osg::Matrix::ortho2D(left, right, bottom, top));
	camera->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
	return camera.release();
}

// **************************************************************************

osgText::Text *CreateText(osgText::Font *font, float fontSize, const osg::Vec3 &pos, const std::string &content, osgText::TextBase::AlignmentType align)

{
	osg::ref_ptr<osgText::Text> text = new osgText::Text;
	if (text.valid() == false)
		return 0;
	text->setFont(font);
	text->setCharacterSize(fontSize);
	text->setPosition(pos);
	text->setAlignment(align);
	text->setAxisAlignment(osgText::TextBase::XY_PLANE);
	text->setColor(osg::Vec4(0, 0, 0, 1.0f));
	text->setText(content);
	return text.release();
}

