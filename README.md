# Stew Organizer

## Make Writing Easier

*Stew* is a tool for organizing novels and other large writing projects. Use it to organize your scenes into chapters, and your chapters into books. Keep track of your characters, settings and other notes, even related files like character sketches. Put it all together in one program and one folder so it's right at your finger tips. Make backups easier. Use your own preferred word processor, so you can work the way *you* want to work.

With *Stew*, your files are on your computer, you can work without being connected, without storing your files in some stranger's cloud. And your document files are kept in whatever format you prefer, allowing you to work the way you want to. Plus, the rest of the settings and properties that *Stew* manages for itself are standard, well-known text-based formats that will still be readable in twenty years, allowing you to still have access to your data even when you don't have *Stew*. Even the document files are named exactly as they appear in the the project tree in *Stew*, so you don't need to look through project files to figure out what ID means what title.

## Install

There is a [debian package available here](https://github.com/user4815162342/stew-gui/releases) which should run on at least Ubunto 12.04 and up. Releases for Windows and Mac OS X are pending.

## Usage

Stew makes use of a folder and file concept which should be familiar to you if you use a computer. When you first open it up, it will ask you for a folder where you can store your project. Everything in your project can be reached through the tree list on the left. Double-clicking on a document opens up the document properties. Here you can create a synopsis, set various attributes to help with workflow and story organization, and click a button to edit the document itself, or a separate file of notes, in your preferred word processor. 

## Migration from Other Systems

If you were previously just managing your project by hand, and already have your project files set up in a folder, you should be able to turn that folder into a stew project quite easily. There might be some edge cases where you'll need to rename some files, but that's about it.

If you are migrating from similar software, such as Scrivener, or my own defunct Story Steward, you will find the process a bit more complex, as this will require reading the old project file syntax, manually changing filenames and editing stew properties. These other programs generally keeps all of their project data in a monolithic file, and even when the documents are separate files, their names are based on unique IDs found in that file, and have nothing to do with what is actually inside. You're welcome to ask for help, as it *has* been done, and there are ways to automate some of the process.

## Technical Details

At it's heart, Stew is basically a wrapper for your file system. It makes use of folders and files to manage content, using some special naming conventions to keep related files together. It allows you to view the entire document, including properties, synopsis, notes, and documents contained within it, as one piece. You can set properties on the document to help you track things like what characters are included in a scene, what events are mentioned, and so on. You can arrange the scenes and chapters into whatever order you want them to be in, or move the scenes around between chapters. 

If you do go into the file system where your documents are stored, you will find it really easy to see how everything is arranged: related files all start with the same name, container documents have a folder which contains files related to its contents. The only things that aren't immediately obvious are the properties and the document order, but these are stored in standard JSON files right next to the document.

Rather than try to re-invent a word processor, which has already been done successfully at least four times in the history of computing, *Stew* allows you to make use of your own, preferred editors (set up through your operating system) for your documents. In fact, you don't even need to edit your files in a word processor, you could use a text editor, or even a drawing program. Stew is designed to organize documents, not worry about what's inside them.

## Future

Stew is designed to be both cross-platform and future-proof from the start. Right now, it has only been released for debian-based Linux, but only a little programming could get it running on other desktop systems. It has also been designed to someday be able to take advantage of cloud-based file systems, so you can store your documents online. There are also many other plans for additional features.
