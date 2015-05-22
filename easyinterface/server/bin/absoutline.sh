#! /bin/bash
let value=0

cat <<EOF
 <category text='Program' selectable='false'>
    <category text='Functions' selectable='false'>
      <category text='append' value='append' selectable='true' />
      <category text='contains' value='contains' selectable='true' />
    </category>
    <category text='Classes' selectable='false'>
      <category text='Server' value='Server' selectable='true'>
        <category text='send' value='Server.send' selectable='true' />
        <category text='receive' value='Server.receive' selectable='true' />
        <category text='register' value='Server.register' selectable='true' />
      </category>
      <category text='Client' value='Client' selectable='true'>
        <category text='send' value='Client.send' selectable='true' />
        <category text='receive' value='Client.receive' selectable='true' />
      </category>
    </category>
    <category text='main' value='main' selectable='true' />
 </category>
EOF
