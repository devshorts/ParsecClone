// StructReader.h

#pragma once

#include <stdio.h>
#include <string.h>

using namespace System;
using namespace std;

namespace CombinatorNative {

	public ref class StructReader abstract sealed
	{

	public:
		generic <typename T> static T Read(array<System::Byte>^ data)
		{
			T value;

			pin_ptr<System::Byte> src = &data[0];
			pin_ptr<T> dst = &value;

			memcpy((void*)dst, (void*)src, sizeof(T));

			return value;
		}

		generic <typename T> static array<T>^ Read(array<array<System::Byte>^>^ data)
		{
			array<T>^ resultList = gcnew array<T>(data->Length);
			
			for(int i = 0; i < data->Length; i++){
				resultList[i] = Read<T>(data[i]);
			}

			return resultList;
		}

		generic <typename T> static array<T>^ Read(array<System::Byte>^ data, int objectSize)
		{
			int numObjects = data->Length / objectSize;

			array<T>^ resultList = gcnew array<T>(numObjects);

			pin_ptr<System::Byte> src = &data[0];
			
			int count = 0;
			for(int i = numObjects - 1; i >= 0 ; i--){
				T value;
				pin_ptr<T> dst = &value;

				memcpy((void*)dst, (void*)(src + (objectSize * i)), sizeof(T));

				resultList[count++] = value;
			}

			return resultList;
		}
	}; 
}
