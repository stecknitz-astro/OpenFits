unit u_const;

{
Date March-01 2020
Author: Frank Szemkus, Von-Parkentin-Str. 2 23919 Berkenthin GERMANY
eMail: kontakt@stecknitz-astronomie.de
Project: OpenFits
Copyright (C) 2020  Frank Szemkus
License: GPL V3

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  ciBit_16 = 65536;
  ciMaxBits = ciBit_16;

  csAppName = 'OpenFits';
  csAuthor = 'Frank Szemkus';
  csEmail = 'kontakt@stecknitz-astronomie.de';

  csVersionMain = '0';
  csVersionSub = '0';
  csVersionSubSub = '1';

type
  TOpenFitsPixFunction = (ofpGamma, ofpHist, ofpCutLowHigh, ofpCutLow, ofpCutHigh, ofpSqrt, ofpPower, ofpMult, ofpAdd, ofpMinus, ofpDiv);

  TFloatColor = record
      rRed,rGreen,rBlue,rAlpha : Real;
    end;

implementation

end.

