unit test_stew_properties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stew_properties, test_registry, sys_file;

type

  { TPropertiesSpec }

  TPropertiesSpec = class(TTestSpec)
  private
    fTestRootDir: TFile;
  public
    procedure SetupTest; override;
  published
    procedure Test_DocumentProperties;
//    procedure Test_Color;
    procedure Test_Keyword;
    procedure Test_CategoryDefinition;
//    procedure Test_KeywordDefinitions;
    procedure Test_KeywordDefinitions_Parsing;
    procedure Test_ProjectProperties;
  end;

implementation

uses
  sys_localfile, FileUtil, sys_json, Graphics, sys_types, sys_dynval;

{ TPropertiesSpec }

procedure TPropertiesSpec.SetupTest;
begin
  inherited SetupTest;
  fTestRootDir := LocalFile(CopyTemporaryFileData('../test-data/story/'));
end;

procedure TPropertiesSpec.Test_DocumentProperties;
var
  lStream: TFileStream;
  lProps: IDocumentProperties;
begin
  lStream := TFileStream.Create(fTestRootDir.GetContainedFile('Chapter 1','properties','json',false).ID,fmOpenRead);
  try
    lProps := TPropertyObjects.NewDocumentProperties;
    lProps.Deserialize(lStream);
    Assert(lProps.Category = 'Chapter','Category should return correct value');
    Assert(lProps.Status = 'Unwritten','Status should return correct value');
    Assert(lProps.Title = 'The Cottage','Title should return correct value');
    Assert(lProps.Publish = false,'Publish should return correct value');
    Assert(lProps.Index.Length = 0,'Index should return correct value');
    Assert(lProps.User['place'].IsEqualTo('Jen''s Lakeside Cottage'),'User properties should return correct values');

  finally
    lStream.Free;
  end;


end;

{procedure TPropertiesSpec.Test_Color;
var
  lColor: TJSColor;
begin
  lColor := TJSColor.Create;
  try
    Assert(lColor.AsColor = clDefault,'Default value for color must be clDefault');
    lColor.Red := $80;
    lColor.Blue := $80;
    lColor.Green := $80;
    Assert(lColor.AsColor = clGray,'Should be able to set RBG separately to get a correct color');

  finally
    lColor.Free;
  end;

end;}

procedure TPropertiesSpec.Test_Keyword;
var
  lKeyword: IKeywordDefinition;
begin
  lKeyword := TPropertyObjects.NewStatusDefinition;
  Assert(lKeyword.Color = clDefault,'Default value for color must be clDefault');
  lKeyword.Color := clBlue;
  Assert(lKeyword.Color = clBlue,'Assigning color property should retain value');

end;

procedure TPropertiesSpec.Test_CategoryDefinition;
var
  lCategory: ICategoryDefinition;
begin
  lCategory := TPropertyObjects.NewCategoryDefinition;
  lCategory.PublishTitle := true;
  Assert(lCategory.PublishTitle,'Publish Title should be correct');
  lCategory.PublishTitleLevel:=2;
  Assert(lCategory.PublishTitleLevel = 2,'Publish Title level should be correct');
  lCategory.PublishTitlePrefix:='R.';
  Assert(lCategory.PublishTitlePrefix = 'R.','Publish title prefix should be correct');
  lCategory.PublishMarkerBefore:=true;
  Assert(lCategory.PublishMarkerBefore,'Publish Marker before should be correct');
  lCategory.PublishMarkerAfter:=true;
  Assert(lCategory.PublishMarkerAfter,'Publish Marker before should be correct');
  lCategory.PublishMarkerBetween:=true;
  Assert(lCategory.PublishMarkerBetween,'Publish Marker before should be correct');

end;

{procedure TPropertiesSpec.Test_KeywordDefinitions;
var
  lDefs: TStatusDefinitions;
  lArray: TJSArray;
begin
  lDefs := TStatusDefinitions.Create;
  try
    lArray := TJSArray.Create;
    try
      lArray.Put(0,'Foo');
      lArray.Put(1,'Bar');
      lDefs.Assign(lArray);
      Assert(lDefs.Get('Bar') is TStatusDefinition,'Assigning array of strings to keyword definition list should create default keyword definitions under the name specified by those strings');
    finally
      lArray.Free;
    end;


  finally
    lDefs.Free;
  end;
end;}

procedure TPropertiesSpec.Test_KeywordDefinitions_Parsing;
const
  lText: UTF8String = '["Unwritten","Written","Final"]';
var
  lDefs: IStatusDefinitions;
begin
  lDefs := TPropertyObjects.NewStatusDefinitions;
  lDefs.Deserialize(lText);
  Assert(lDefs.Has('Unwritten'),'Parsing an array of strings should create a mapped KeywordDefinition in KeywordDefinitions object [1]');
  Assert(lDefs['Unwritten'].Color = clDefault,'Parsing an array of strings should create a mapped KeywordDefinition in KeywordDefinitions object [2]');

end;

procedure TPropertiesSpec.Test_ProjectProperties;
var
  lStream: TFileStream;
  lProps: IProjectProperties;
  lDeadlines: IDeadlines;
begin
  lStream := TFileStream.Create(fTestRootDir.GetContainedFile('','stew','json',true).ID,fmOpenRead);
  try
    lProps := TPropertyObjects.NewProjectProperties;
      lProps.Deserialize(lStream);
      Assert(lProps.Categories.Keys.Count > 0,'Combination of parsing and GetPath should have gotten some data');
      Assert(lProps.Categories['Chapter'].PublishTitleLevel = 0,'Project category definitions should work');
      Assert(lProps.Categories.Has('Scene'),'Project categories should work');
      Assert(lProps.DefaultCategory = 'Chapter','Project default category should work');
      Assert(lProps.DefaultDocExtension = 'txt','Project default doc extension should work');
      // (sic) I want the default notes extension to be weird, for template testing later.
      Assert(lProps.DefaultNotesExtension = 'tst','Project default notes extension should work');
      Assert(lProps.DefaultStatus = 'Unwritten','Project default status should work');
      Assert(lProps.DefaultThumbnailExtension = 'png','Project default thumbnail extension should work');
      Assert(lProps.Statuses['Unwritten'].Color = clRed,'Project statuses should work');
      lProps.DefaultDocExtension := '.doc';
      Assert(lProps.DefaultDocExtension = 'doc','Default doc extension should trim off the "." when assigning');


      lDeadlines := lProps.Deadlines;
      lDeadlines.Add('First Chapter Done',ISO8601ToDateTime('2015-11-08T17:01:00'));
      lDeadlines.Add('All Done',ISO8601ToDateTime('2072-01-28T05:25:36'));
      Assert(lDeadlines[0].Name = 'First Chapter Done','Deadline name should be stored correctly');
      Assert(lDeadlines[1].Due = ISO8601ToDateTime('2072-01-28T05:25:36'),'Deadline due should be stored correctly.');


  finally
    lStream.Free;
  end;

end;

end.

