```sh
dotnet restore
dotnet publish -c Release
cp app.yaml bin/Release/netcoreapp2.1/publish
gcloud app deploy bin/Release/netcoreapp2.1/publish --project=<project_id>
```