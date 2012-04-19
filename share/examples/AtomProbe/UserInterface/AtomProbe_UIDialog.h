/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#ifndef TankPlugin_UIDIALOG_H
#define TankPlugin_UIDIALOG_H

#include <QtGui/QWidget>

 #include <QtGui/QGraphicsView>
 #include <QtGui/QGraphicsScene>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/OneDStringArray.h>
#include <ves/xplorer/command/CommandManager.h>
#include <ves/conductor/qt/UITabs.h>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SignalWrapper.h>
#include <vector>
//#include <QtCore/QString>
typedef boost::signals2::signal< void ( const std::vector< double >&, const std::vector< double >&  ) > TwoDoubleVectorSignal_type;
typedef boost::signals2::signal< void ( const std::vector< int >&, const std::vector< int >&, const std::vector< int >&, const std::vector< bool >&  ) > ColorBoolVectorSignal_type;



namespace Ui {
    class AtomProbe_UIDialog;
}

class AtomProbe_UIDialog : public QWidget
{
    Q_OBJECT

public:
    explicit AtomProbe_UIDialog(QWidget *parent = 0);
    ~AtomProbe_UIDialog();

protected:
    void changeEvent(QEvent *e);

	
	void updateColorizeHistogram(std::vector<int> r, std::vector<int> g,std::vector<int> b, std::vector<double> values);
	void updateMaskHistogram(std::vector<double> values);
	void setNumPulses(double pulses) {
		numPulses = pulses;
		numPulsesSignal();
	}
	signals:
	void colorizeHistogramSignal();
	void maskHistogramSignal();
	void numPulsesSignal();
	
	protected slots:
	void qtUpdateColorizeHistogram();
	void qtUpdateMaskHistogram();
	void qtUpdateNumPulses();
	
protected slots:
	void enableLattice();
	void enableDislocations();
	void toggleAnimation();
	void disableAll();
	void setFrame(int frame);
	void setData(int data);
	void setTop(double top);
	void setBottom(double bottom);
	void setMaskData(int data);
	void setMaskTop(double top);
	void setMaskBottom(double bottom);
	void setLastTime(int frame);
	
	
	void setHit0Enabled(int state);
	void setHit1Enabled(int state);
	void setHit2Enabled(int state);
	void setHit3Enabled(int state);
	void setHit0Color();
	void setHit1Color();
	void setHit2Color();
	void setHit3Color();
	/*void setHit0R(int r);
	void setHit0G(int g);
	void setHit0B(int b);
	
	void setHit0Enabled(int state);
	void setHit0R(int r);
	void setHit0G(int g);
	void setHit0B(int b);
	
	void setHit0Enabled(int state);
	void setHit0R(int r);
	void setHit0G(int g);
	void setHit0B(int b);
	
	void setHit0Enabled(int state);
	void setHit0R(int r);
	void setHit0G(int g);
	void setHit0B(int b);*/
	
	
	
private:
    Ui::AtomProbe_UIDialog *ui;
	
	QGraphicsScene * colorizeHistogramScene, *maskHistogramScene;
	ves::util::IntSignal_type m_SelectColorizeDataSignal;
	ves::util::IntSignal_type m_SetMaskVariableSignal;
	ves::util::DoubleSignal_type m_SetMaskBottomSignal;
	ves::util::DoubleSignal_type m_SetLastTimeSignal;
	ves::util::DoubleSignal_type m_SetMaskTopSignal;
	ColorBoolVectorSignal_type m_SetHitColorizeSignal;
	
	std::vector<double> curValues, curMaskValues;
	std::vector<int> rr, gg, bb;
	std::vector<QString> labelText;
	std::vector<int> rrHit, ggHit, bbHit;
	double numPulses;
	std::vector<bool> hitEnabled;
	/// Required to be able to connect up to signals.
    ves::xplorer::eventmanager::ScopedConnectionList m_connections;
};

#endif // TankPlugin_UIDIALOG_H
