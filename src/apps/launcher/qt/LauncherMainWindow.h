/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#ifndef LAUNCHERMAINWINDOW_H
#define LAUNCHERMAINWINDOW_H

#include <QtGui/QMainWindow>
#include <QtGui/QTextEdit>
#include <QtCore/QProcess>

namespace Ui {
    class LauncherMainWindow;
}

class LauncherMainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit LauncherMainWindow(QWidget *parent = 0);
    ~LauncherMainWindow();

protected:
    void changeEvent(QEvent *e);

private:
    Ui::LauncherMainWindow *ui;
    QTextEdit* m_stdout;
    QProcess* m_process;

private slots:
    void on_launch_clicked();
    void on_m_workingDirButton_clicked();
    void on_m_configurationButton_clicked();
    void onReadyReadStandardOutput();
};

#endif // LAUNCHERMAINWINDOW_H
