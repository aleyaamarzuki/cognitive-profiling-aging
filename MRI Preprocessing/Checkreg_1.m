%% Check co-registration with MNI template. Script will open raw mprage
% and MNI template and allow you to make adjustments. 
clc;
clear classes;

%change this to your path
data_path = 'C:\Users\aleya\OneDrive\Documents\MRI\MRI_workshop\';

%get foldernames
files1 = dir(data_path);

%remove elements that are not participant folders
matches = regexp({files1([files1.isdir]).name}, {'^\d+|^A\d+'});
bool = cellfun(@(x) any(x), matches);
files = files1(bool);

%get SPM12
%change this to your path
addpath 'C:\Users\aleya\OneDrive\Documents\spm12\';
spm

%Script folder
%change this to your path
addpath 'C:\Users\aleya\OneDrive\Documents\MRI\Scripts\'


%carry out co-registration

for i = 1:length(files)

subnum = files(i).name; %get names of each subject's folder

matlabbatch{1}.spm.util.checkreg.data = {
                                         [data_path num2str(subnum) '\' num2str(subnum) '.nii,1']
                                         'C:\Users\aleya\Documents\spm12\canonical\avg152T1.nii,1'%file to coregister against
                                         };

spm_jobman('run', matlabbatch);

disp('Paused for reorientation. Click on the Command Window and press Enter once you have saved the participants file to continue')
pause;

disp('Continuing on to next subject')
end 


