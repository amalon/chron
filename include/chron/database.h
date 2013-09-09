/*
 * chron/database.h
 *
 * Copyright (C) 2012 James Hogan <james@albanarts.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details
 * (in the file called COPYING).
 *
 *
 * Class for a chron database of chronological facts and rules.
 *
 */

#ifndef _CHRON_DATABASE_H_
#define _CHRON_DATABASE_H_

#include "common.h"

namespace chron
{
	class Database
	{
		PlEngine *_engine;
	public:
		Database(const char *arg0, const char *file)
		{
			char *args[] = {
				(char *)arg0,
				(char *)"-q",	// silence SWI-prolog copyright information

				(char *)"-p",
				(char *)"chron=../src/",

				(char *)"-s",
				(char *)"../src/wrapper.pl",

				(char *)"--",
				(char *)"--file",
				(char *)file,
			};
			_engine = new PlEngine(sizeof(args)/sizeof(args[0]),
					       args);
		}

		~Database()
		{
			delete _engine;
		}
	};
};

#endif // _CHRON_DATABASE_H_
