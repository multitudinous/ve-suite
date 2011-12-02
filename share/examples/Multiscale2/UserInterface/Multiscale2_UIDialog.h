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
 * Date modified: $Date: 2011-10-07 16:20:34 -0500 (Fri, 07 Oct 2011) $
 * Version:       $Rev: 16404 $
 * Author:        $Author: tjordan $
 * Id:            $Id: TankPlugin_UIDialog.h 16404 2011-10-07 21:20:34Z tjordan $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef TankPlugin_UIDIALOG_H
#define TankPlugin_UIDIALOG_H

#include <QtGui/QWidget>


#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/OneDStringArray.h>
#include <ves/xplorer/command/CommandManager.h>
#include <ves/conductor/qt/UITabs.h>


namespace Ui {
    class Multiscale2_UIDialog;
}

class Multiscale2_UIDialog : public QWidget
{
    Q_OBJECT

public:
    explicit Multiscale2_UIDialog(QWidget *parent = 0);
    ~Multiscale2_UIDialog();

protected:
    void changeEvent(QEvent *e);
	

protected slots:
	void enableLattice();
	void enableDislocations();
	void toggleAnimation();
	void disableAll();
	void setFrame(int frame);

private:
    Ui::Multiscale2_UIDialog *ui;
};

#endif // TankPlugin_UIDIALOG_H
